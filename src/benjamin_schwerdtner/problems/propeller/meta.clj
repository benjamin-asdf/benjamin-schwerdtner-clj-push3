(ns benjamin-schwerdtner.problems.propeller.meta
  "The Propeller problems as a landscape for dual evolution.

  ## Why this pairing

  `mutation/dual.clj` ends with an admission:

      So: this landscape cannot answer whether meta evolution pays. It
      saturates, and it is stationary - and a STATIONARY landscape is
      precisely the case where evolvability has nothing to buy you.
      Barnett/Meister/Rainey got selection for evolvability out of an
      environment that KEPT CHANGING [...] Testing this loop needs a
      landscape with that shape.

  Program synthesis under DOWNSAMPLING has that shape, and gets it honestly
  rather than by adding noise. Each generation scores the population against a
  fresh random subset of the training cases, so:

  - the landscape moves every generation, and it moves the way a real
    environment does - by changing which competences are currently being
    tested, not by perturbing a scalar;
  - it does not saturate. The `name the largest integer` landscape ran out at
    the INTEGER clamp; `fizz-buzz` has no ceiling short of a correct program;
  - it is the standard method, not a contrivance. Helmuth & Spector's
    down-sampled lexicase is how PushGP is normally run, so the moving
    landscape costs nothing in realism.

  ## Reading the result

  `dual.clj` selects M on the improvement rate it induces, and the standing
  finding in this repository is that IMPROVEMENT-EVENT COUNT IS DECOUPLED FROM
  OBJECTIVE PROGRESS - a change can lift M's credit metric while the held-out
  benchmark stays flat, and one run reached a better benchmark with HALF the
  improvement events. So the numbers to read here are:

    `:benchmark`     total error on the FULL training set, over the whole
                     population, never used in selection. This is the only
                     number that means progress.
    `:solved`        cases solved exactly by the best individual, which is
                     what the benchmark suite is actually asking for.

  and NOT `:improvements`, which is reported only to show whether M's credit
  signal had any density at all.

  ## The three conditions

  Comparing metaevolution against nothing is not a result. [[compare-conditions]]
  runs, on the same problem, the same seed and the same case draws:

    `:meta`      M evolves         (`:m-replace-frac 0.3`)
    `:frozen`    M fixed, still competing for events (`:m-replace-frac 0`)
    `:gp`        no M at all - lexicase + UMAD, `gp.clj`

  `:frozen` is the one that matters. It is what distinguishes \"evolving the
  mutation operators helped\" from \"having a bag of twenty hand-written
  operators helped\", and in the busy-beaver runs it was enough to erase the
  effect."
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
   [benjamin-schwerdtner.clj-push3-play.mutation.dual :as dual]
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [benjamin-schwerdtner.problems.propeller.gp :as gp]
   [benjamin-schwerdtner.problems.propeller.problems :as problems]
   [benjamin-schwerdtner.problems.propeller.run :as prun]
   [random-seed.core :as rs :refer [rand rand-int rand-nth shuffle]]))

(def defaults
  {;; Cases per generation. Small on purpose - it is what makes the landscape
   ;; move, and it is also the whole cost of a fitness evaluation, which here
   ;; is the dominant cost of the run.
   :downsample 12
   :n-train 100
   :n-test 200
   ;; Below Propeller's 2000. A program is executed
   ;; (e-size + events x n-variations) times per generation, so this number
   ;; multiplies into everything.
   :step-limit 600
   :e-size 30
   :m-size 26
   :n-variations 2
   :events-per-generation 60
   :e-max-points 250
   :max-initial-points 60
   :generations 40
   :seed 0})

(defn landscape
  "A `dual/run` config for `problem`.

  The two functions that matter:

  `fitness` is the error on the CURRENT case subset, so it moves. Lower is
  better, hence `better? <`.

  `benchmark` is the error on the full training set. It is held out of
  selection entirely - `dual/snapshot` takes it over the whole population and
  reports it, and nothing reads it back. Under a moving landscape a
  generation's `:best-fitness` is scored against a world that changed, so
  comparing it across generations means nothing; the benchmark is the only
  yardstick that survives."
  [problem opts]
  (let [{:as config :keys [downsample n-train n-test step-limit
                           max-initial-points seed]}
        (merge defaults opts)
        alphabet (problems/alphabet problem)
        atom-generators (problems/atom-generators problem)
        {:keys [train test]} (problems/train-and-test
                              problem n-train n-test seed)
        ;; The subset this generation is judged on. An atom rather than a
        ;; parameter because `dual/step` owns the loop; `before-generation` is
        ;; the hook it offers for exactly this.
        current (atom (vec (take downsample train)))]
    {:config
     (merge
      (select-keys config [:e-size :m-size :n-variations
                           :events-per-generation :e-max-points])
      {:alphabet alphabet
       :better? <
       :seed seed
       ;; The landscape moves, so last generation's memoised fitnesses are
       ;; scores against last generation's world. dual/step drops the cache
       ;; each generation when this is false.
       :stationary-fitness? false

       :before-generation
       (fn [{:keys [generation]}]
         ;; Seeded from the generation number so the sequence of case subsets
         ;; is a pure function of `seed` - a run has to be repeatable even
         ;; though its landscape moves.
         (binding [rs/*rng* (rs/random (+ (* 31 seed) generation))]
           (reset! current (vec (take downsample (shuffle train))))))

       :fitness
       (fn [program]
         (reduce + 0.0 (prun/errors problem program @current
                                    {:step-limit step-limit})))

       :benchmark
       (fn [program]
         (reduce + 0.0 (prun/errors problem program train
                                    {:step-limit step-limit})))

       :random-genome
       (fn [] (gp/random-genome atom-generators max-initial-points))})
     :train train
     :test test}))

(defn run-meta
  "Dual evolution on `problem`. Returns the run report.

  `:m-replace-frac 0` freezes M, which is the control condition."
  [problem opts]
  (let [{:keys [generations] :as opts} (merge defaults opts)
        {:keys [config train test]} (landscape problem opts)
        config (merge config (select-keys opts [:m-replace-frac :credit-mode
                                                :cost-mode :m-mutator]))
        step-limit (:step-limit opts)
        ;; :exceptions-are-warnings? is set inside the interpreter for each
        ;; evaluation, and evolved mutators produce a great deal of absorbed
        ;; noise on *err*.
        result (binding [*err* (java.io.StringWriter.)]
                 (dual/run generations config))
        ;; `dual/snapshot` keeps the best individual by RELATIVE fitness,
        ;; which under a moving landscape is not the objectively best one. The
        ;; final population is re-scored on the full sets through the same
        ;; function the GP reports through.
        report (prun/population-report problem (map :program (:e result))
                                       train test {:step-limit step-limit})]
    (merge
     {:problem (:name problem)
      :condition (if (zero? (get config :m-replace-frac
                                 (:m-replace-frac dual/defaults)))
                   :frozen
                   :meta)
      :generations generations
      :best-mutator (:best-mutator (last (:history result)))
      :history (mapv #(select-keys % [:generation :benchmark :best-fitness
                                      :improvements :children :m-distinct])
                     (:history result))}
     report
     {:points (util/count-points (:best-program report))})))

(defn run-baseline
  "The same problem under lexicase + UMAD, sized to the same budget.

  Equal generations is NOT equal compute, and pretending otherwise would
  flatter whichever side got more. `dual` performs
  `e-size + events x n-variations` evaluations per generation; the GP performs
  `population-size`. `:population-size` is set to match, so both conditions
  spend approximately the same number of program executions."
  [problem opts]
  (let [{:as opts :keys [e-size events-per-generation n-variations
                         generations downsample n-train n-test step-limit
                         max-initial-points e-max-points seed]}
        (merge defaults opts)]
    (gp/run-gp problem
               (merge
                {:population-size (+ e-size (* events-per-generation n-variations))
                 :max-generations generations
                 :max-initial-points max-initial-points
                 :max-points e-max-points
                 :n-train n-train
                 :n-test n-test
                 :step-limit step-limit
                 ;; The same moving landscape, so the comparison is about the
                 ;; search and not about who had the easier problem.
                 :downsample-rate (/ (double downsample) n-train)
                 :seed seed}
                ;; GP-only knobs passed straight through, so a caller can vary
                ;; the baseline's own algorithm while keeping the budget and
                ;; the landscape fixed. Without this the map above silently
                ;; DROPPED them: a `:selection :tournament` run came back with
                ;; numbers identical to lexicase, because it had quietly been
                ;; a lexicase run.
                (select-keys opts [:selection :tournament-size :umad-rate
                                   :variation :elitism])))))

(defn compare-conditions
  "Run `:meta`, `:frozen` and `:gp` on `problem` at one seed.

  Returns the three reports plus a summary. Read `:benchmark` and
  `:train-solved`; see the namespace docstring on why not `:improvements`."
  [problem opts]
  (let [meta-r (run-meta problem (assoc opts :m-replace-frac 0.3))
        frozen-r (run-meta problem (assoc opts :m-replace-frac 0))
        gp-r (run-baseline problem opts)]
    {:problem (:name problem)
     :seed (:seed (merge defaults opts))
     :summary {:meta (select-keys meta-r [:benchmark :train-solved :test-solved])
               :frozen (select-keys frozen-r [:benchmark :train-solved :test-solved])
               :gp (select-keys gp-r [:train-solved :test-solved])}
     :meta meta-r
     :frozen frozen-r
     :gp gp-r}))

;; ---------------------------------------------------------------------------
;; The experiment
;; ---------------------------------------------------------------------------

(defn experiment
  "`compare-conditions` over several problems and seeds.

  Several seeds is not optional. The busy-beaver runs in `dual.clj` found the
  run-to-run spread larger than any difference between conditions, so a
  single-seed result here would be uninterpretable in exactly the same way."
  [problem-names seeds opts]
  (vec
   (for [pname problem-names
         seed seeds
         :let [problem (problems/by-name pname)
               t0 (System/currentTimeMillis)
               ;; Progress on stdout, flushed. A full experiment is tens of
               ;; minutes across eighteen independent runs, and without this
               ;; a failure in one of them is indistinguishable from the job
               ;; still working.
               _ (do (println "==" pname "seed" seed) (flush))
               r (compare-conditions problem (assoc opts :seed seed))
               _ (do (println "  " (pr-str (:summary r))) (flush))]]
     (assoc (dissoc r :meta :frozen :gp)
            :seconds (/ (- (System/currentTimeMillis) t0) 1000.0)
            ;; The full reports are large - histories and genomes - so only
            ;; what a comparison needs is kept.
            :detail {:meta (dissoc (:meta r) :history :best-program)
                     :frozen (dissoc (:frozen r) :history :best-program)
                     :gp (dissoc (:gp r) :history :best-program :config)}
            :benchmark-trajectories
            {:meta (mapv (juxt :generation :benchmark) (:history (:meta r)))
             :frozen (mapv (juxt :generation :benchmark)
                           (:history (:frozen r)))}))))

(defn run-experiment!
  "Run [[experiment]] and write it to `path`.

  Writes after EVERY problem-seed rather than once at the end. A full
  experiment is around forty minutes across eighteen independent runs, and
  writing only on completion means any failure - or an impatient kill - throws
  away every run that had already finished."
  [path problem-names seeds opts]
  (let [header {:opts opts :problems problem-names :seeds seeds}
        done (atom [])
        write! (fn [] (spit path (pr-str (assoc header :results @done))))]
    (doseq [pname problem-names
            seed seeds]
      (swap! done into (experiment [pname] [seed] opts))
      (write!))
    (println "wrote" path (str "(" (count @done) " runs)"))
    @done))

(defn summarise
  "Per-problem means of the three conditions, from an [[experiment]] result."
  [results]
  (into (sorted-map)
        (for [[pname rs] (group-by :problem results)]
          [pname
           (into {}
                 (for [c [:meta :frozen :gp]]
                   [c {:train-solved
                       (double (/ (reduce + (map #(get-in % [:detail c :train-solved] 0) rs))
                                  (count rs)))
                       :test-solved
                       (double (/ (reduce + (map #(get-in % [:detail c :test-solved] 0) rs))
                                  (count rs)))}]))])))

(comment
  (require '[benjamin-schwerdtner.problems.propeller.problems :as p])

  ;; One condition, to see the benchmark trajectory and how dense M's credit
  ;; signal is.
  (def r (run-meta (p/by-name :fizz-buzz) {:generations 10 :seed 1}))
  (mapv (juxt :generation :benchmark :improvements) (:history r))
  (select-keys r [:benchmark :train-solved :test-solved :best-mutator])

  ;; All three, on one problem, one seed. Slow.
  (compare-conditions (p/by-name :fizz-buzz) {:generations 20 :seed 1}))
