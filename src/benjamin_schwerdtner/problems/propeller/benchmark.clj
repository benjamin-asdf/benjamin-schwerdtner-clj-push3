(ns benjamin-schwerdtner.problems.propeller.benchmark
  "The whole suite, under four searches, paying one currency.

  Chaitin's comparison is of RATES: hold the landscape fixed and ask what each
  search regime costs to get a given distance. `metabiology/evolution.clj` does
  that for busy beavers; this does it for the 31 PSB problems, adding the
  population-based baseline as a fourth condition:

      exhaustive-search      enumerate programs shortest first
      cumulative-evolution   random algorithmic mutation, keep if fitter
      intelligent-design     inspect a pool of 32, take the best child
      gp                     lexicase + UMAD over a population

  ## The currency

  One TRIAL is one program evaluated against the case set. That is
  `evolution.clj`'s unit, and it is the only way the four are comparable: the
  three regimes hill-climb one organism and the GP carries a population, so
  generations and mutation events are not commensurable but program
  evaluations are.

  For the GP that means `population-size x generations` = `:trials`, and the
  population is sized to whatever makes that identity hold. Every condition
  also sees the SAME fixed case set - no downsampling anywhere - because the
  regimes have no population to downsample across and a moving target would
  make `:solved` stop being a progress axis.

  ## What the comparison can and cannot show

  It can show the rate separation Chaitin's mathematics is about, on a
  landscape that does not saturate trivially.

  It cannot separate SEARCH REGIME from SELECTION METHOD, and that limit is
  worth stating because the previous experiment here found selection to be the
  dominant variable (docs/propeller-recook.md: swapping lexicase for scalar
  tournament accounted for ~83% of the GP's advantage over dual evolution).
  The three regimes are single-organism hill-climbers, i.e. the extreme of
  scalar selection, so any GP win confounds `has a population` with `selects
  per case`. Read a regime-vs-regime difference as being about search; read
  regime-vs-GP as being about both."
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [benjamin-schwerdtner.problems.propeller.gp :as gp]
   [benjamin-schwerdtner.problems.propeller.metabiology :as mb]
   [benjamin-schwerdtner.problems.propeller.problems :as problems]
   [benjamin-schwerdtner.problems.propeller.run :as prun]))

(def defaults
  {:trials 20000
   ;; The metabiology notes are emphatic that this landscape has a takeoff
   ;; between 4000 and 15000 trials and that "at 4000 trials every frugal
   ;; policy looks like total failure". 20000 is the smallest budget that is
   ;; past that warning.
   :fitness :neg-error
   ;; `:solved` is the faithful analogue of busy-beaver fitness, and it is
   ;; unusable as a default here: an exact solve is rare, so the landscape is
   ;; flat almost everywhere and nothing is ever accepted. Measured on
   ;; small-or-large, `:solved` accepted 0 mutations in 20000 trials where
   ;; `:neg-error` accepted 2 and reached 50/100. Kept as a condition rather
   ;; than a default - the flatness is a finding, not a bug.
   :n-cases 25
   :step-limit 600
   :n-train 100
   :n-test 200
   :pool-size 32
   ;; Population for the GP, chosen so population x generations = trials.
   :population-size 200
   :seeds [1 2 3]})

(defn- gp-condition
  "The GP at the same trial budget and on the same fixed case set."
  [problem {:keys [trials population-size n-cases step-limit n-train n-test seed]}]
  (let [generations (max 1 (quot trials population-size))
        r (gp/run-gp problem
                     {:population-size population-size
                      :max-generations generations
                      :max-initial-points 60
                      :max-points 400
                      :n-train n-train
                      :n-test n-test
                      :step-limit step-limit
                      ;; The regimes score on `n-cases` of the training set, so
                      ;; the GP is held to the same fixed subset rather than a
                      ;; fresh downsample per generation.
                      :downsample-rate (/ (double n-cases) n-train)
                      :seed seed})]
    {:problem (:name problem)
     :regime :gp
     :seed seed
     :trials (* population-size generations)
     :train-solved (:train-solved r)
     :n-train (:n-train r)
     :test-solved (:test-solved r)
     :n-test (:n-test r)
     :points (util/count-points (:best-program r))
     ;; As a STRING. A results file has to be readable back, and a program
     ;; that ever contains a non-printable atom makes the whole edn
     ;; unreadable - which happened, via ERC generator functions.
     :best-program (pr-str (:best-program r))}))

(defn run-one
  "One problem, one seed, all four conditions."
  [problem opts]
  (let [{:as opts :keys [trials fitness n-cases step-limit n-train n-test
                         pool-size seed]}
        (merge defaults opts)
        regime-opts {:trials trials :fitness fitness :n-cases n-cases
                     :step-limit step-limit :n-train n-train :n-test n-test
                     :pool-size pool-size :seed seed}]
    (conj (mapv (fn [regime]
                  (-> (mb/run-regime problem regime regime-opts)
                      (dissoc :trajectory)
                      (update :best-program pr-str)))
                [:exhaustive-search :cumulative-evolution :intelligent-design])
          (gp-condition problem opts))))

(defn run-all
  "The suite. `pmap` over problem-seed pairs; each run binds its own RNG, so
  parallelism does not change any result."
  [problem-names opts]
  (let [{:as opts :keys [seeds]} (merge defaults opts)
        jobs (for [pname problem-names, seed seeds] [pname seed])]
    (vec
     (apply concat
            (pmap (fn [[pname seed]]
                    (let [t0 (System/currentTimeMillis)
                          rs (try
                               (run-one (problems/by-name pname)
                                        (assoc opts :seed seed))
                               (catch Throwable e
                                 [{:problem pname :seed seed
                                   :error (str (.getName (class e)) ": "
                                               (ex-message e))}]))]
                      (println "==" pname seed
                               (str (int (/ (- (System/currentTimeMillis) t0) 1000)) "s")
                               (pr-str (mapv (juxt :regime :train-solved) rs)))
                      (flush)
                      rs))
                  jobs)))))

(defn summarise
  "Mean training cases solved per regime per problem, and the head-to-head."
  [results]
  (let [ok (remove :error results)
        by-problem (group-by :problem ok)
        regimes [:exhaustive-search :cumulative-evolution :intelligent-design :gp]]
    {:per-problem
     (into (sorted-map)
           (for [[pname rs] by-problem]
             [pname (into {}
                          (for [rg regimes
                                :let [xs (filter #(= rg (:regime %)) rs)]
                                :when (seq xs)]
                            [rg (double (/ (reduce + (map #(or (:train-solved %) 0) xs))
                                           (count xs)))]))]))
     :errors (mapv (juxt :problem :seed :error) (filter :error results))}))

(defn head-to-head
  "Wins, ties and losses between two regimes, counted per run rather than
  averaged - the seed-to-seed spread on these problems is larger than most
  differences between conditions, so a mean of three is not a result."
  [results a b]
  (let [idx (group-by (juxt :problem :seed) (remove :error results))]
    (frequencies
     (keep (fn [[_ rs]]
             (let [ra (first (filter #(= a (:regime %)) rs))
                   rb (first (filter #(= b (:regime %)) rs))]
               (when (and ra rb)
                 (cond (> (:train-solved ra) (:train-solved rb)) a
                       (< (:train-solved ra) (:train-solved rb)) b
                       :else :tie))))
           idx))))

(defn run-benchmark!
  "Run the suite and write it to `path`, incrementally."
  [path problem-names opts]
  (let [done (atom [])
        header {:opts (merge defaults opts) :problems problem-names}
        write! (fn [] (spit path (pr-str (assoc header :results @done))))]
    ;; Chunks of 3 rather than 4: at 4 (12 concurrent runs) two jobs
    ;; died of OutOfMemoryError inside a 12G cgroup.
    (doseq [chunk (partition-all 3 problem-names)]
      (swap! done into (run-all chunk opts))
      (write!))
    (println "wrote" path (str "(" (count @done) " rows)"))
    (flush)
    @done))

(comment
  (require '[benjamin-schwerdtner.problems.propeller.problems :as p])
  (run-one (p/by-name :small-or-large) {:trials 5000 :seed 1})
  (run-benchmark! "results/propeller-benchmark.edn" (mapv :name p/all-problems) {}))
