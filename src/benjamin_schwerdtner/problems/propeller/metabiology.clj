(ns benjamin-schwerdtner.problems.propeller.metabiology
  "Chaitin's three regimes, on program synthesis instead of busy beavers.

  `metabiology/evolution.clj` compares three search regimes paying one
  currency - trials:

      exhaustive search      enumerate programs shortest first
      cumulative evolution   random algorithmic mutation, keep if fitter
      intelligent design     inspect a pool, take the best child

  Its landscape is \"name the largest integer\", which it reaches by BIGNUM
  and which SATURATES: once an organism hits the bit budget nothing can be
  strictly fitter and the run is over. The metabiology notes are explicit that
  this is a statement about the substrate rather than about evolution.

  The PSB benchmarks are the same shape with the ceiling made meaningful. Cases
  solved plays the role of bits named: it is the thing being maximised, it has
  a ceiling, and reaching the ceiling really is the end of the search - the
  problem is solved. So the mapping is

      bits named        ->  training cases solved exactly
      saturated?        ->  every case solved, i.e. the benchmark is beaten
      bit budget        ->  the number of cases
      program points    ->  unchanged, and still what frugality prices

  and `:bits` in a trajectory record is the cases-solved count, because that is
  the generic progress axis the regime machinery reads.

  ## What carries over unchanged, and why that matters

  The regimes themselves. `evolution.clj` takes `:observe` and `:alphabet` as
  options now, so cumulative-evolution here is the SAME function, the same
  loop, and the same trials accounting as the one that produced
  `results/metabiology-regimes.edn`. Nothing about the comparison had to be
  reimplemented, which is the only way the two sets of numbers mean the same
  thing.

  Algorithmic mutation carries over too, and it is the substantive import.
  `mutation/mutate` applies an evolved PUSH PROGRAM to the genome, so mutation
  distance is program-size complexity rather than Hamming distance -
  `:mut_duplicate` doubles a genome in one step, which point mutation reaches
  with probability 2^-k. That is Chaitin's point, and it is a different
  variation operator from the UMAD used by `gp.clj`.

  ## What does not carry over

  Population. All three regimes hill-climb a SINGLE organism, so there is no
  lexicase selection and no diversity - and the recorded finding on these
  problems is that E's selection method dominates everything else
  (docs/propeller-recook.md). A single-organism regime is the extreme case of
  a scalar selection method, so the expectation going in is that it loses to
  the GP. Worth measuring precisely because it is a clean instance of the same
  axis.

  Also: `:code_rand` splices ~80% junk, per the metabiology notes. The mutators
  here draw from `mutation/mutator-alphabet`, which uses `:mb_rand_atom`
  against the PROBLEM's alphabet instead."
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [benjamin-schwerdtner.metabiology.evolution :as evolution]
   [benjamin-schwerdtner.problems.propeller.problems :as problems]
   [benjamin-schwerdtner.problems.propeller.run :as prun]
   [random-seed.core :as rs]))

;; ---------------------------------------------------------------------------
;; Fitness policies
;;
;; Deliberately the same shapes as `organism/fitness-policies`, so the frugality
;; findings recorded there can be checked here rather than re-derived. The
;; metabiology result was that cost pressure helps only when WEAK: ratio
;; frugality (`named/points`) buys leanness without freezing the climb, while
;; proportional log-space penalties freeze it outright.
;; ---------------------------------------------------------------------------

(def fitness-policies
  {:solved
   {:doc "Cases solved exactly. The analogue of :busy-beaver - maximise the
   thing with a ceiling, and stop when the ceiling is reached."
    :saturation-terminal? true
    :f (fn [{:keys [solved]}] solved)}

   :neg-error
   {:doc "Minus the total error. A FINER gradient than :solved, which is why it
   is here: on most of these problems an exact solve is rare and a policy that
   can only see exact solves is climbing a landscape that is flat almost
   everywhere. This has no analogue in the busy-beaver setting, where the
   number named is already continuous."
    :saturation-terminal? false
    :f (fn [{:keys [total-error]}] (- total-error))}

   :frugal-size
   {:doc "solved / program points.

   The metabiology runs settled on this shape, and the REASON THEY GAVE FOR IT
   DOES NOT TRANSFER. There the numerator is the number named - a tower of
   exponentials - so it dominates any plausible program size, every real
   improvement is accepted whatever it costs, and the denominator only
   separates organisms naming the SAME number. That is why ratio frugality
   bought leanness without freezing the climb.

   Here the numerator is a case count, bounded by `:n-cases`. A 4-solver of 100
   points scores 0.04 and a 3-solver of 3 points scores 1.0, so the denominator
   dominates instead and the ratio becomes a strong proportional penalty -
   which is the shape the metabiology notes identify as freezing a search
   outright. Kept because measuring that is the point of porting the policies,
   but do not expect it to behave as it does on busy beavers; `:parsimonious`
   is the policy that keeps the intended meaning on a bounded score."
    :saturation-terminal? false
    :f (fn [{:keys [solved points]}] (/ (double solved) (max 1 points)))}

   :parsimonious
   {:doc "Solve more first; among equals prefer the shorter program. Parsimony
   as tie-break rather than penalty, so neutral drift survives."
    :saturation-terminal? false
    :f (fn [{:keys [solved points]}] [solved (- points)])}})

(defn policy
  [k]
  (or (get fitness-policies k)
      (throw (ex-info "Unknown fitness policy"
                      {:policy k :known (keys fitness-policies)}))))

;; ---------------------------------------------------------------------------
;; Observing an organism on a benchmark problem
;; ---------------------------------------------------------------------------

(defn make-observe
  "An `:observe` for `evolution.clj`, scoring programs on `cases`.

  Returns the keys the regime machinery reads - `:fitness`, `:bits`, `:steps`,
  `:halted?`, `:saturated?` - plus `:solved` and `:total-error` for reading.
  `:bits` IS `:solved`; see the namespace docstring on why the progress axis
  keeps that name."
  [problem cases {:keys [fitness step-limit]
                  :or {fitness :solved step-limit 600}}]
  (let [f (:f (policy fitness))
        n (count (prun/errors problem '() cases {:step-limit step-limit}))]
    (fn [program _organism-opts]
      (let [errs (prun/errors problem program cases {:step-limit step-limit})
            solved (count (filter zero? errs))
            ctx {:solved solved
                 :total-error (reduce + 0.0 errs)
                 :points (util/count-points program)}]
        (assoc ctx
               :fitness (f ctx)
               :bits solved
               ;; The regimes report these; a synthesis organism has no
               ;; meaningful halting story separate from its step limit, so
               ;; `:steps` is left at the limit rather than invented.
               :steps step-limit
               :halted? true
               :saturated? (= solved n))))))

(defn search-alphabet
  "The problem's alphabet as a flat vector of ATOMS a program may contain.

  `problems/alphabet` mixes three things: instruction keywords, literal ERCs,
  and ERC GENERATORS - `random-int` is a function, not a value. The GP never
  sees that difference, because `problems/atom-generators` calls a generator
  to produce a fresh constant every time it draws one. The regimes take the
  alphabet directly, so handing them the raw vector meant `:mb_rand_atom`
  spliced the FUNCTION OBJECT itself into genomes: inert when executed, and
  one fewer usable constant than the GP had.

  That is not a cosmetic difference. It gave the population-based condition
  access to random integer constants that the three single-organism regimes
  could not reach, on exactly the comparison this namespace exists to make.

  Each generator is therefore sampled into `n-samples` concrete values, drawn
  from the run's seeded RNG, so the alphabet is still a function of the seed."
  [problem n-samples]
  (into []
        (mapcat (fn [a]
                  (if (fn? a)
                    (repeatedly n-samples a)
                    [a])))
        (problems/alphabet problem)))

(def defaults
  {:trials 20000
   :fitness :solved
   :step-limit 600
   :n-train 100
   :n-test 200
   ;; Cases the organism is scored on. The regimes hill-climb one organism, so
   ;; there is no population to downsample across - every trial sees the same
   ;; cases, which is what makes `:solved` a stable progress axis.
   :n-cases 25
   :pool-size 32
   ;; Concrete values drawn from each ERC generator, see [[search-alphabet]].
   :erc-samples 4
   :seed 0})

(defn run-regime
  "One regime on one problem. `regime` is :cumulative-evolution,
  :intelligent-design or :exhaustive-search."
  [problem regime opts]
  (let [{:as opts :keys [trials fitness step-limit n-train n-test n-cases
                         pool-size seed erc-samples]}
        (merge defaults opts)
        {:keys [train test]} (problems/train-and-test problem n-train n-test seed)
        cases (vec (take n-cases train))
        observe (make-observe problem cases {:fitness fitness
                                             :step-limit step-limit})
        alphabet (binding [rs/*rng* (rs/random seed)]
                   (search-alphabet problem (or erc-samples 4)))
        f (case regime
            :cumulative-evolution evolution/cumulative-evolution
            :intelligent-design evolution/intelligent-design
            :exhaustive-search evolution/exhaustive-search)
        result (binding [*err* (java.io.StringWriter.)]
                 (f {:trials trials
                     :seed seed
                     :start '()
                     :observe observe
                     :alphabet alphabet
                     :saturation-terminal? (:saturation-terminal? (policy fitness))
                     :pool-size pool-size
                     ;; What :mb_rand_atom draws from, so an inserted atom is
                     ;; one the PROBLEM can use.
                     :mutation {:alphabet alphabet}
                     :organism {}}))
        best (:program (:best result))]
    {:problem (:name problem)
     :regime regime
     :fitness fitness
     :seed seed
     :trials (:trials result)
     :accepted (:accepted result)
     :stopped (:stopped result)
     ;; Re-scored on the FULL training and test sets, the same way `gp.clj` and
     ;; `meta.clj` report, so all four searches are read off one ruler.
     :solved-on-cases (:bits (:best result))
     :n-cases (count cases)
     :train-solved (prun/n-solved problem best train)
     :n-train (count (prun/errors problem '() train))
     :test-solved (prun/n-solved problem best test)
     :n-test (count (prun/errors problem '() test))
     :points (:points (:best result))
     :best-program best
     :trajectory (mapv #(select-keys % [:trials :accepted :bits :points])
                       (:trajectory result))}))

(comment
  (require '[benjamin-schwerdtner.problems.propeller.problems :as p])

  ;; One problem, the Darwinian regime.
  (-> (run-regime (p/by-name :small-or-large) :cumulative-evolution
                  {:trials 2000 :seed 1})
      (dissoc :best-program :trajectory))

  ;; The comparison Chaitin's mathematics is about: hold the target fixed and
  ;; read the cost per regime.
  (for [r [:exhaustive-search :cumulative-evolution :intelligent-design]]
    (-> (run-regime (p/by-name :small-or-large) r {:trials 2000 :seed 1})
        (select-keys [:regime :trials :accepted :solved-on-cases]))))
