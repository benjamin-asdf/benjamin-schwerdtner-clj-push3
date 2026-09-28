(ns benjamin-schwerdtner.problems.propeller.run
  "Executing a Push program against a Propeller problem's cases.

  This is the layer both the baseline GP and the metaevolution loop sit on, so
  it is deliberately thin: given a program and a list of cases, produce the
  per-case errors. Everything about HOW to search lives above it.

  The only subtlety is reading the answer back out. A Propeller problem names
  the stack its answer is on, and four shapes occur:

  - a value stack (`:push/integer`, `:push/string`, ...): the top item;
  - `:push/print`: the accumulated printed output, which is a string and is
    never missing, because the PRINT accumulator starts at \"\";
  - `:push/integer-pair` (only `find-pair`): the top TWO integers;
  - `:push/output` (the multipliers): the register bank, `:num-outputs` wide.

  A program that leaves nothing on its output stack yields `:no-stack-item`,
  which every error function in `problems.clj` charges the same large penalty
  for. That is the same convention as Propeller's `state/peek-stack`. The
  register bank is the exception - it always reads out full width, with
  `:unset` for registers never written, so a missing bit costs one bit rather
  than the whole case."
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.instructions.input-output :as io]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.problems.propeller.problems :as problems]))

(def default-step-limit
  "EXEC pops a program is allowed per case.

  Propeller's benchmark configurations use `:step-limit 200` for the PSB
  problems and 2000 for a few. 2000 is used here as the default because a
  program that loops over a twenty-element vector needs more than 200 pops
  before it has done anything, and 200 silently makes the vector problems
  unsolvable rather than hard."
  2000)

(defn- read-output
  [state output num-outputs]
  (case output
    :push/print (io/printed state)
    :push/integer-pair (let [s (get-in state [:stacks :push/integer])]
                         (if (<= 2 (count s))
                           ;; Top of stack first, so that a program pushing
                           ;; `a` then `b` answers [a b] - the order the
                           ;; expected pair is written in.
                           [(nth s (- (count s) 2)) (peek s)]
                           :no-stack-item))
    ;; The circuit benchmarks: the answer is the register bank, not a stack.
    ;; Always a full-width vector, with `:unset` for registers the program
    ;; never wrote - so a missing bit is scored as a wrong bit rather than
    ;; collapsing the whole case to one no-output penalty.
    :push/output (io/output-registers state num-outputs)
    (stack/peek-item state output)))

(defn execute
  "Run `program` on one input tuple, return the value on `output`."
  [program inputs {:keys [output step-limit num-outputs input-names]
                   :or {step-limit default-step-limit}}]
  (try
    (-> (push/setup-state)
        ;; A runaway instruction inside a fitness evaluation is a bad program,
        ;; not a bug in the run - the whole point of evolving over a Turing
        ;; complete substrate is that most candidates are broken.
        (assoc :exceptions-are-warnings? true)
        ;; `:input-names` binds the tuple to the problem's own names instead
        ;; of `:in1` .., which is what the multiplier benchmarks want: `:a2`
        ;; says which bit it is, and that is the difference between an
        ;; alphabet a circuit can be written in and six anonymous inputs.
        (io/with-inputs (if input-names
                          (zipmap input-names inputs)
                          inputs))
        (push/execute program {:max-executions step-limit})
        (read-output output num-outputs))
    (catch Throwable _ :no-stack-item)))

(defn errors
  "Per-case errors of `program` on `cases`.

  Returns the error vector lexicase selection needs - it selects on the
  VECTOR, not on the total.

  Usually one entry per case. A problem whose error function returns several
  numbers per case is FLATTENED, so the multiplier benchmarks select on
  individual output BITS: their 64 cases become 384 errors. That is Propeller's
  behaviour and it is the whole point of using lexicase on them - a program
  that gets the low bit right on every input is worth keeping even if the rest
  of the product is wrong, and a per-case error would average that away."
  ([problem program cases] (errors problem program cases {}))
  ([problem program cases opts]
   (let [err (problems/error-fn problem)
         opts (merge (select-keys problem [:output :num-outputs :input-names]) opts)]
     (into []
           (mapcat (fn [{:keys [inputs output]}]
                     (let [e (err output (execute program inputs opts))]
                       (if (sequential? e) e [e]))))
           cases))))

(defn total-error
  [problem program cases]
  (reduce + 0.0 (errors problem program cases)))

(defn n-solved
  "How many of `cases` the program gets exactly right.

  Reported alongside total error because total error is dominated by the
  no-output penalty and so says little about progress; the count of solved
  cases is the quantity the benchmark is actually about."
  [problem program cases]
  (count (filter zero? (errors problem program cases))))

(defn evaluate
  "A full report of `program` on `cases`, for inspection rather than search."
  [problem program cases]
  (let [err (problems/error-fn problem)
        opts (select-keys problem [:output :num-outputs :input-names])]
    (mapv (fn [{:keys [inputs output]}]
            (let [actual (execute program inputs opts)]
              {:inputs inputs
               :expected output
               :actual actual
               :error (err output actual)}))
          cases)))

;; ---------------------------------------------------------------------------
;; Reporting a run
;; ---------------------------------------------------------------------------

(defn population-report
  "How a final population did on the full training and test sets.

  Both search algorithms in this port judge their population against a
  DOWNSAMPLED case subset that changes every generation, so neither one's
  internal notion of \"best\" is a statement about the full problem. This
  re-scores the whole final population on the full sets, and is the single
  place either algorithm's result is turned into a number - which is what
  makes the comparison in meta.clj an equal one.

  Two statistics, and each is the best of ITS OWN kind over the population:

    `:benchmark`     the lowest total error
    `:train-solved`  the most cases solved exactly

  They usually come from DIFFERENT individuals, and collapsing them - taking
  the cases-solved of the lowest-error individual - understates a run badly.
  Measured: a fizz-buzz population containing a program that solved 17 of 100
  cases reported 0, because the individual with the lowest total error solved
  none of them exactly."
  [problem programs train test opts]
  (let [;; Distinct, because a population under truncation selection fills up
        ;; with copies of its best, and re-scoring each copy on the full
        ;; training set is the expensive part of a run's bookkeeping.
        programs (vec (distinct programs))
        ;; One pass. Both statistics come off the same error vectors, so
        ;; computing them separately would double the cost of the report.
        scored (mapv (fn [p]
                       (let [errs (errors problem p train opts)]
                         {:program p
                          :total (reduce + 0.0 errs)
                          :solved (count (filter zero? errs))
                          :n-errors (count errs)}))
                     programs)]
    (if (empty? scored)
      {:benchmark nil :best-program nil :train-solved 0
       :n-train (count train) :test-solved 0 :n-test (count test)}
      (let [best-error (apply min-key :total scored)
            best-solver (apply max-key :solved scored)]
        {:benchmark (:total best-error)
         :best-program (:program best-error)
         :train-solved (:solved best-solver)
         ;; The number of ERRORS, not of cases. They differ only for the
         ;; multiplier benchmarks, where one case contributes one error per
         ;; output bit - and there `:train-solved` counts correct bits, so the
         ;; denominator has to match it or the fraction is nonsense.
         :n-train (:n-errors (first scored))
         :n-train-cases (count train)
         :best-solver (:program best-solver)
         ;; Generalisation of the individual that solved the most TRAINING
         ;; cases. Choosing the individual on the training set and then
         ;; reporting its test score is the honest order; picking the best
         ;; test scorer would be selecting on the test set.
         :test-solved (n-solved problem (:program best-solver) test)
         :n-test (count test)}))))

;; ---------------------------------------------------------------------------
;; Checking the port itself
;; ---------------------------------------------------------------------------

(defn check-reference-solution
  "Confirm a problem's reference solution runs on all its cases.

  Not a test of the solution's correctness - nothing here can know that - but
  of the problem being WELL FORMED: that `:gen-inputs` produces tuples
  `:solution` accepts, that its arity matches, and that no generator loops
  forever. Those are the failure modes of a hand-written data generator, and
  they present as a benchmark that is mysteriously unsolvable."
  [problem n seed]
  (try
    (let [cases (benjamin-schwerdtner.problems.propeller.data/cases
                 problem n seed)]
      {:name (:name problem)
       :ok? true
       :n (count cases)
       :arity-ok? (every? #(= (:arity problem) (count (:inputs %))) cases)
       :distinct-outputs (count (distinct (map :output cases)))
       :sample (first cases)})
    (catch Throwable e
      {:name (:name problem) :ok? false :error (ex-message e)})))

(comment
  (require '[benjamin-schwerdtner.problems.propeller.problems :as p])

  ;; Every problem well formed?
  (remove :ok? (mapv #(check-reference-solution % 50 1) p/all-problems))

  ;; A hand-written solution to fizz-buzz, to prove the machinery end to end.
  (let [problem (p/by-name :fizz-buzz)
        cases (:train (p/train-and-test problem 20 20 1))]
    (n-solved problem '(:in1 15 :integer_mod 0 :integer_=) cases)))
