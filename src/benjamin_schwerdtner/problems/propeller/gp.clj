(ns benjamin-schwerdtner.problems.propeller.gp
  "Propeller's own search algorithm, on this Push3 implementation.

  This is the CONTROL. Its job is to be the standard thing - lexicase
  selection, UMAD variation, subtree crossover - so that a claim about
  metaevolution (`problems/propeller/meta.clj`) has something to be measured
  against. Anything clever belongs there, not here.

  ## Lexicase selection

  Propeller's default parent selection, and the reason PushGP works on these
  problems at all. Tournament selection compares TOTAL error, so a program
  that is brilliant on the FizzBuzz cases and hopeless on the rest loses to a
  mediocre generalist, and the specialist's insight is thrown away. Lexicase
  instead shuffles the cases and filters the population by exact-best error on
  each in turn: a program that is uniquely best on even one case survives to
  reproduce. It selects for specialists and lets the population hold several
  incompatible partial solutions at once.

  That is also why `run/errors` returns a VECTOR and not a sum - the sum is
  the thing lexicase exists to avoid.

  ## UMAD

  Uniform Mutation by Addition and Deletion, at `umad-rate`. Propeller applies
  it to a flat genome; genomes here are trees, so it walks the tree and
  applies the same two operations at each position. The deletion rate is
  `r/(1+r)` and not `r`, which is Propeller's normalisation and is what keeps
  expected genome length constant - with equal rates a genome grows without
  bound, and growth alone improves fitness for a while because a longer
  program has more chances to leave something on the output stack.

  ## Downsampling

  `:downsample-rate` evaluates each generation against a random subset of the
  training cases. Helmuth & Spector's result is that spending the same total
  evaluations on more generations against fewer cases beats fewer generations
  against all of them. It also makes the fitness landscape NON-STATIONARY,
  which matters for the metaevolution experiment and is discussed there."
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [benjamin-schwerdtner.problems.propeller.problems :as problems]
   [benjamin-schwerdtner.problems.propeller.run :as run]
   [random-seed.core :as rs :refer [rand rand-int rand-nth shuffle]]))

;; ---------------------------------------------------------------------------
;; Variation
;; ---------------------------------------------------------------------------

(defn random-genome
  [atom-generators max-points]
  (gen/generate-code
   {:max-points max-points
    :atom-generators atom-generators
    :branch-probability (constantly 0.15)
    :close-probability (fn [n] (/ (- 1 (/ 1 n)) 3))}))

(defn umad
  "Uniform mutation by addition and deletion.

  Walks the tree; at each element, inserts a fresh random atom beside it with
  probability `rate`, and deletes it with probability `rate/(1+rate)`. Nested
  lists are recursed into, so structure is preserved rather than flattened."
  [program atom-generators rate]
  (let [delete-rate (/ rate (+ 1.0 rate))
        step (fn step [node]
               (if-not (seq? node)
                 node
                 (apply list
                        (mapcat
                         (fn [item]
                           (let [item (if (seq? item) (step item) item)]
                             (concat
                              ;; Addition first, so a freshly added atom is
                              ;; not immediately a candidate for deletion.
                              (when (< (rand) rate)
                                [(gen/rand-atom atom-generators)])
                              (when-not (< (rand) delete-rate)
                                [item]))))
                         node))))]
    (step program)))

(defn crossover
  "Subtree crossover: a random subtree of `b` replaces a random subtree of `a`.

  Propeller crosses flat genomes uniformly. On a tree the structure-preserving
  analogue is subtree exchange, which is also the classical GP operator.

  Both index functions take a depth-first point index and wrap it themselves,
  so a plain random integer is a valid position."
  [a b]
  (let [donor (util/code-at-point b (rand-int (util/count-points b)))
        result (util/insert-code-at-point a donor
                                          (rand-int (util/count-points a)))]
    ;; Replacing the ROOT point yields the donor itself, which may be an atom.
    ;; A genome has to be a program, so an atom is wrapped back into one.
    (if (seq? result) result (list result))))

(defn make-variation
  "One offspring from `parents`, under Propeller's `:variation` mix."
  [{:keys [atom-generators umad-rate variation max-points]}]
  (fn [select]
    (let [r (rand)
          child (if (< r (:crossover variation 0.5))
                  (crossover (select) (select))
                  (umad (select) atom-generators umad-rate))]
      ;; Nothing else bounds genome size, and crossover in particular can
      ;; double it in one step. An oversized child is discarded in favour of
      ;; its parent rather than truncated - truncating would hand selection a
      ;; program neither operator actually produced.
      (if (and child (<= (util/count-points child) max-points))
        child
        (select)))))

;; ---------------------------------------------------------------------------
;; Selection
;; ---------------------------------------------------------------------------

(defn lexicase-select
  "One parent, by lexicase selection over `:errors`.

  `population` is a vector of `{:program p :errors [...]}`."
  [population]
  (loop [survivors population
         cases (shuffle (range (count (:errors (first population)))))]
    (if (or (empty? cases) (= 1 (count survivors)))
      (:program (rand-nth survivors))
      (let [c (first cases)
            best (reduce min (map #(nth (:errors %) c) survivors))]
        (recur (filterv #(= best (nth (:errors %) c)) survivors)
               (rest cases))))))

;; ---------------------------------------------------------------------------
;; Epsilon lexicase
;;
;; Plain lexicase filters on EXACT equality with the best error on a case.
;; On integer and string problems that is right - exact ties are the normal
;; case, and being tied with the best is meaningful. On CONTINUOUS errors it
;; is a bug: two programs essentially never have bit-identical floating point
;; errors, so the first case filters the pool down to a single individual and
;; lexicase silently degenerates into "best on one random case". Every bit of
;; its multi-case selection pressure is gone.
;;
;; Epsilon lexicase keeps everything within `epsilon` of the best, where
;; epsilon is the case's MEDIAN ABSOLUTE DEVIATION over the population
;; (Spector et al.; La Cava et al. 2016). MAD rather than a fixed tolerance
;; because the right tolerance is a property of how spread out the population
;; happens to be on that case, which changes as the run converges.
;;
;; This matters for the six float-output problems here - bouncing-balls,
;; dice-game, shopping-list, snow-day, nguyen-f1, nguyen-f5 - and Propeller
;; configures `:epsilon-lexicase` for exactly that reason on nguyen-f1.
;; ---------------------------------------------------------------------------

(defn median
  [coll]
  (let [sorted (vec (sort coll))
        n (count sorted)]
    (when (pos? n)
      (if (odd? n)
        (nth sorted (quot n 2))
        (/ (+ (nth sorted (dec (quot n 2))) (nth sorted (quot n 2))) 2.0)))))

(defn median-absolute-deviation
  [coll]
  (when-let [m (median coll)]
    (median (map #(abs (- % m)) coll))))

(defn epsilon-list
  "Per-case epsilon: the median absolute deviation of that case's errors
  across the population."
  [population]
  (let [n (count (:errors (first population)))]
    (mapv (fn [c] (or (median-absolute-deviation
                       (map #(nth (:errors %) c) population))
                      0.0))
          (range n))))

(defn epsilon-lexicase-select
  "One parent, by epsilon-lexicase over `:errors` with per-case `epsilons`."
  [population epsilons]
  (loop [survivors population
         cases (shuffle (range (count (:errors (first population)))))]
    (if (or (empty? cases) (= 1 (count survivors)))
      (:program (rand-nth survivors))
      (let [c (first cases)
            errs (map #(nth (:errors %) c) survivors)
            best (reduce min errs)
            eps (nth epsilons c 0.0)]
        (recur (filterv #(<= (abs (- (nth (:errors %) c) best)) eps) survivors)
               (rest cases))))))

(defn tournament-select
  "One parent by tournament on total error. Kept as the control FOR lexicase -
  the claim that lexicase matters on these problems should be checkable."
  [population size]
  (:program
   (reduce (fn [a b] (if (< (:total b) (:total a)) b a))
           (repeatedly (max 1 size) #(rand-nth population)))))

;; ---------------------------------------------------------------------------
;; The loop
;; ---------------------------------------------------------------------------

(def defaults
  {:population-size 200
   ;; Propeller's benchmark runs use 1000 x 300. That is ~60M program
   ;; executions per problem per run, which is a cluster job rather than a
   ;; REPL one. These defaults are sized to say something in minutes; a real
   ;; comparison needs the bigger numbers and more than one seed.
   :max-generations 100
   :max-initial-points 100
   :max-points 400
   :umad-rate 0.1
   :variation {:umad 0.5 :crossover 0.5}
   ;; `:lexicase`, `:epsilon-lexicase` or `:tournament`. A problem whose
   ;; errors are continuous should say `:epsilon-lexicase` in its own map -
   ;; see the note above [[epsilon-lexicase-select]] for why plain lexicase is
   ;; not merely suboptimal there but degenerate.
   :selection :lexicase
   :tournament-size 5
   :n-train 200
   :n-test 500
   ;; nil = use every training case every generation.
   :downsample-rate nil
   :step-limit run/default-step-limit
   ;; false, which is what Propeller's own benchmark configurations use. Worth
   ;; being explicit about, because it is one axis on which the comparison in
   ;; meta.clj is NOT controlled: dual evolution's survival step keeps the top
   ;; `e-size` of parents and offspring together, which is elitist with
   ;; respect to the current landscape, while this is generational
   ;; replacement. They are different algorithms and that difference is part
   ;; of what they are; equal compute, the same landscape and the same
   ;; reporting are what the comparison does control.
   :elitism false
   :seed 0})

(defn- evaluated
  [problem programs cases]
  (mapv (fn [p]
          (let [errs (run/errors problem p cases)]
            {:program p :errors errs :total (reduce + 0.0 errs)}))
        programs))

(defn run-gp
  "Evolve a solution to `problem`. Returns the final state with a `:history`.

  A run stops early on a program that solves every TRAINING case; whether it
  generalises is then a separate question, which is what `:test-solved` in the
  result answers."
  [problem opts]
  (let [{:as config :keys [population-size max-generations max-initial-points
                           n-train n-test seed downsample-rate selection
                           tournament-size]}
        ;; A problem may name its own selection method - Propeller sets
        ;; `:parent-selection` per problem file, and for the continuous-error
        ;; ones that choice is part of the problem being well posed rather
        ;; than a tuning preference. It beats [[defaults]] and loses to an
        ;; explicit `opts`, so a caller can still override it to run a control.
        (merge defaults
               (when-let [s (:selection problem)] {:selection s})
               opts)
        atom-generators (problems/atom-generators problem)
        config (assoc config :atom-generators atom-generators)
        {:keys [train test]} (problems/train-and-test problem n-train n-test seed)
        vary (make-variation config)]
    (binding [rs/*rng* (rs/random seed)
              ;; `run/execute` sets :exceptions-are-warnings?, so every
              ;; instruction an evolved program calls with the wrong stack
              ;; contents - mod by a value it cannot use, subs past the end -
              ;; prints its stack trace. Those are ORDINARY EVENTS under
              ;; evolution, not diagnostics: a 40-generation run emitted half
              ;; a megabyte of them, interleaved into the experiment log and
              ;; drowning the progress lines. `meta/run-meta` already did
              ;; this for the dual-evolution side; without it here, only the
              ;; GP condition was noisy, which is a confusing asymmetry.
              *err* (java.io.StringWriter.)]
      (loop [generation 0
             programs (vec (repeatedly
                            population-size
                            #(random-genome atom-generators max-initial-points)))
             history []]
        (let [;; The cases THIS generation is judged on. Under downsampling
              ;; this is a fresh subset every generation, so a parent's error
              ;; vector is not comparable across generations - which is the
              ;; point, and is why the report below always re-scores the best
              ;; program on the full training set.
              cases (if downsample-rate
                      (vec (take (max 1 (long (* downsample-rate (count train))))
                                 (shuffle train)))
                      train)
              pop (evaluated problem programs cases)
              best (reduce (fn [a b] (if (< (:total b) (:total a)) b a)) pop)
              train-solved (run/n-solved problem (:program best) train)
              snapshot {:generation generation
                        :best-total (:total best)
                        :best-points (util/count-points (:program best))
                        ;; Always on the FULL training set, never on the
                        ;; downsample - otherwise a lucky easy subset reads as
                        ;; progress.
                        :train-solved train-solved
                        :n-train (count train)
                        :mean-points (double (/ (reduce + (map (comp util/count-points :program) pop))
                                                (count pop)))}
              history (conj history snapshot)
              solved? (= train-solved (count train))]
          (if (or solved? (>= generation max-generations))
            ;; `best` above is the best on THIS GENERATION'S CASES, which
            ;; under downsampling says nothing about the full problem. The
            ;; result is `run/population-report` over the final population -
            ;; the same function `meta/run-meta` reports through, which is
            ;; what makes the two comparable.
            (merge {:problem (:name problem)
                    :condition :gp
                    :config (dissoc config :atom-generators)
                    :generation generation
                    :solved? solved?
                    :history history}
                   (run/population-report problem (map :program pop)
                                          train test
                                          {:step-limit (:step-limit config)}))
            (let [select (case selection
                           :lexicase #(lexicase-select pop)
                           ;; The epsilons are a property of THIS generation's
                           ;; population, so they are computed once here and
                           ;; closed over, not recomputed per selection - one
                           ;; MAD per case per parent would dominate the cost
                           ;; of the generation.
                           :epsilon-lexicase (let [eps (epsilon-list pop)]
                                               #(epsilon-lexicase-select pop eps))
                           :tournament #(tournament-select pop tournament-size))
                  n-children (if (:elitism config)
                               (dec population-size)
                               population-size)]
              (recur (inc generation)
                     (into (if (:elitism config) [(:program best)] [])
                           (repeatedly n-children #(vary select)))
                     history))))))))

(comment
  (require '[benjamin-schwerdtner.problems.propeller.problems :as p])

  ;; The cheapest problem in the suite, as a smoke test of the whole stack.
  (def r (run-gp (p/by-name :integer-regression)
                 {:population-size 100 :max-generations 20 :n-train 21 :n-test 21}))
  (select-keys r [:solved? :generation :train-solved :test-solved :best-program])
  (mapv (juxt :generation :best-total :train-solved) (:history r)))
