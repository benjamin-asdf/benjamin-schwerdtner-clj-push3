(ns benjamin-schwerdtner.metabiology.evolution
  "The three regimes, which are the point of Chaitin's mathematics.

  Metabiology's central result is not that software evolves - it is a
  *comparison of rates*. Take organisms whose fitness is the number they name
  (busy beaver), and ask how long each search regime needs to reach BB(N):

      exhaustive search      ~2^N time      - stupid, exponential
      cumulative evolution   ~N^2 mutations - Darwin: random walk, keep if fitter
      intelligent design     ~N mutations   - an oracle picks the best mutation

  Cumulative evolution sits strictly between blind search and design, and much
  closer to design than to blindness. That is the mathematical content of \"life
  is creative\": Darwinian evolution is a genuine, quantifiable search
  accelerator, not a mechanical process, and not magic either.

  This namespace runs all three on the same substrate so the curves can be
  compared. Each regime returns a trajectory of record-holders, which is the
  growth curve of the organism's mathematical ability.

  ------------------------------------------------------------------------
  Accounting - the one thing worth getting right
  ------------------------------------------------------------------------

  The regimes are only comparable if they pay the same currency. We charge
  `:trials`, one per mutation attempt or per candidate program evaluated:

    exhaustive search    1 trial per program enumerated
    cumulative evolution 1 trial per mutation attempt
    intelligent design   `:pool-size` trials per step, because we do not have
                         Chaitin's oracle and must *buy* the best mutation by
                         sampling and evaluating a pool

  That last line is a deliberate handicap relative to the theorem. Chaitin's
  designer gets the best mutation for free, which is what makes it ~N; ours
  pays pool-size per step. If you want the theorem's ID, read
  `:accepted` (the number of accepted mutations) rather than `:trials`.

  ------------------------------------------------------------------------
  Honest limits
  ------------------------------------------------------------------------

  - Fitness here is the *bounded* busy beaver (step budget, bit budget), which
    is computable. So the strict theorem - fitness outgrowing every computable
    function - is not what these runs demonstrate. What they demonstrate is the
    separation between the regimes, which is the part you can actually run.
  - `:saturated?` in a result means the organism hit the substrate's bit
    budget, not the limit of its own cleverness. A regime whose best organism
    saturates has finished the experiment, not the mathematics: raise
    `:max-bignum-bits` and it keeps going. That is the open-endedness, and it
    is why there is no \"converged\" state to report.
  - Exhaustive search enumerates *flat* programs only, so it cannot express a
    multi-instruction :exec_y loop body (which needs nesting) while the mutation
    regimes can build one. It is handicapped on expressiveness on top of being
    exponential. Fixing that means enumerating nested programs in size order;
    the exponential blowup is the same either way."
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require
   [benjamin-schwerdtner.metabiology.organism :as organism]
   [benjamin-schwerdtner.metabiology.mutation :as mutation]
   [clojure.math.combinatorics :as combo]
   [random-seed.core :refer [rand rand-int rand-nth random *rng*]]))

(def default-opts
  {:trials 20000
   :seed 1
   ;; Chaitin's organisms start from nothing. See generator.clj note #4.
   :start '()
   :organism {:max-executions 200
              :max-bignum-bits 10000
              :oracle :resource-bounded}
   :mutation {}
   ;; intelligent-design only: how many mutations the "designer" inspects
   :pool-size 32
   ;; ---------------------------------------------------------------------
   ;; What is being evolved.
   ;;
   ;; The three regimes are a statement about SEARCH, not about busy beavers:
   ;; random-walk-and-keep-if-fitter versus pool-best versus enumeration, all
   ;; paying the same currency. Nothing in the loops below is specific to
   ;; naming large numbers - only `observe` and the alphabet are. Injecting
   ;; them lets the same regime code, and therefore the same accounting, run
   ;; over a different landscape; `problems/propeller/metabiology.clj` uses
   ;; this to put Chaitin's comparison on program synthesis.
   ;;
   ;; `observe`   program, organism-opts -> a map with at least
   ;;             :fitness (compared with organism/fitter?, higher is better)
   ;;             :bits    the progress axis, whatever counts as getting
   ;;                      further - bits named here, cases solved there
   ;;             :steps :halted? :saturated?
   ;; `alphabet`  what exhaustive-search enumerates over.
   ;; `saturation-terminal?`  overrides the fitness policy's own answer, for
   ;;             an observe that has no organism fitness policy behind it.
   ;; ---------------------------------------------------------------------
   :observe organism/observe
   :alphabet organism/alphabet
   ;; Once an organism saturates the bit budget nothing can be strictly fitter,
   ;; so the walk is over - not because it converged, but because the substrate
   ;; ran out. Stopping there makes time-to-saturation the honest headline number.
   :stop-on-saturation? true})

(defn- observe
  [program opts]
  ((:observe opts organism/observe) program (:organism opts)))

(defn- record
  "A trajectory entry: what was named, when, at what cost, and by whom."
  [trials obs program]
  {:trials trials
   :bits (:bits obs)
   :halted? (:halted? obs)
   :saturated? (:saturated? obs)
   :steps (:steps obs)
   :points (organism/size program)
   :program program})

(defn- result
  ([regime opts trials accepted best-obs best trajectory]
   (result regime opts trials accepted best-obs best trajectory :budget-exhausted))
  ([regime opts trials accepted best-obs best trajectory stopped]
   {:regime regime
    :trials trials
    :accepted accepted
    :stopped stopped
    :best (record trials best-obs best)
    :trajectory trajectory
    :opts (select-keys opts
                       [:seed :trials :pool-size :organism :mutation
                        :stop-on-saturation?])}))

(defn- saturation-terminal?
  "Does hitting the bit budget actually end the search under this fitness policy?

  Under `:busy-beaver` it does - nothing can be strictly fitter than a saturated
  organism. Under the frugal policies it does not: a saturated organism can
  still improve by naming the same number with less program or fewer steps, and
  that continuation is the most interesting thing frugal fitness does."
  [opts]
  (if (contains? opts :saturation-terminal?)
    (:saturation-terminal? opts)
    (:saturation-terminal?
     (organism/policy (get (:organism opts) :fitness :busy-beaver)))))

(defn- done?
  "Should the walk stop here? Either the budget is gone or the substrate is."
  [{:as opts :keys [trials stop-on-saturation?]} n obs]
  (cond
    (<= trials n) :budget-exhausted
    (and stop-on-saturation?
         (saturation-terminal? opts)
         (:saturated? obs)) :saturated))

;; ---------------------------------------------------------------------------
;; 1. Cumulative evolution - Chaitin's model of Darwin
;;
;; A single organism. Draw a mutation from the algorithmic distribution, apply
;; it, keep the child if it names a bigger number. This is a hill-climbing
;; random walk - and note R. Watson's objection, recorded in ideas/BIG_IDEAS.org:
;; natural selection may be divide-and-conquer rather than hill climbing. This
;; regime implements Chaitin's version, not Watson's.
;; ---------------------------------------------------------------------------

(defn cumulative-evolution
  ([] (cumulative-evolution {}))
  ([opts]
   (let [{:as opts :keys [trials seed start mutation]} (merge default-opts opts)]
     (binding [*rng* (random seed)]
       (let [start-obs (observe start opts)]
         (loop [n 0
                accepted 0
                parent start
                parent-obs start-obs
                traj [(record 0 start-obs start)]]
           (if-let [stopped (done? opts n parent-obs)]
             (result :cumulative-evolution opts n accepted parent-obs parent traj
                     stopped)
             (let [m (mutation/random-mutator mutation)
                   child (mutation/mutate parent m mutation)
                   n (inc n)]
               (if (nil? child)
                 (recur n accepted parent parent-obs traj)
                 (let [child-obs (observe child opts)]
                   (if (organism/fitter? (:fitness child-obs)
                                         (:fitness parent-obs))
                     (recur n (inc accepted) child child-obs
                            (conj traj (assoc (record n child-obs child)
                                              :accepted (inc accepted)
                                              :mutator m
                                              :log2-probability
                                              (mutation/log2-probability m mutation))))
                     (recur n accepted parent parent-obs traj))))))))))))

;; ---------------------------------------------------------------------------
;; 2. Intelligent design - the designer, bought rather than given
;;
;; At each step inspect a pool of candidate mutations and take the best child.
;; Chaitin's designer is an oracle and gets this for free (hence ~N mutations);
;; we pay `:pool-size` trials per step.
;; ---------------------------------------------------------------------------

(defn intelligent-design
  ([] (intelligent-design {}))
  ([opts]
   (let [{:as opts :keys [trials seed start mutation pool-size]}
         (merge default-opts opts)]
     (binding [*rng* (random seed)]
       (let [start-obs (observe start opts)]
         (loop [n 0
                accepted 0
                parent start
                parent-obs start-obs
                traj [(record 0 start-obs start)]]
           (if-let [stopped (done? opts n parent-obs)]
             (result :intelligent-design opts n accepted parent-obs parent traj
                     stopped)
             (let [pool (into []
                              (keep (fn [_]
                                      (let [m (mutation/random-mutator mutation)]
                                        (when-let [child (mutation/mutate parent m mutation)]
                                          {:mutator m
                                           :program child
                                           :obs (observe child opts)}))))
                              (range pool-size))
                   n (+ n pool-size)
                   ;; `reduce` with organism/fitter?, not `max-key`: fitness
                   ;; is only guaranteed COMPARABLE, not numeric. The
                   ;; :parsimonious policies return [named -points] vectors,
                   ;; and max-key threw ClassCastException on them - so the
                   ;; designer regime could not be run under exactly the
                   ;; policies that were written to tame its bloat.
                   ;; `reduce` with organism/fitter?, not `max-key`: fitness
                   ;; is only guaranteed COMPARABLE, not numeric. The
                   ;; :parsimonious policies return [named -points] vectors,
                   ;; and max-key threw ClassCastException on them - so the
                   ;; designer regime could not be run under exactly the
                   ;; policies written to tame its bloat.
                   ;;
                   ;; Tie-breaking matches `max-key` deliberately: it keeps a
                   ;; candidate only if it is STRICTLY fitter, so ties go to
                   ;; the later pool member. Getting that backwards silently
                   ;; rewrites which child is taken whenever a pool contains
                   ;; equals, which on this landscape is most of the time, and
                   ;; the recorded busy-beaver numbers stop reproducing.
                   winner (when (seq pool)
                            (reduce (fn [a b]
                                      (if (organism/fitter? (:fitness (:obs a))
                                                            (:fitness (:obs b)))
                                        a b))
                                    pool))]
               (if (and winner
                        (organism/fitter? (:fitness (:obs winner))
                                          (:fitness parent-obs)))
                 (recur n (inc accepted) (:program winner) (:obs winner)
                        (conj traj (assoc (record n (:obs winner) (:program winner))
                                          :accepted (inc accepted)
                                          :mutator (:mutator winner)
                                          :log2-probability
                                          (mutation/log2-probability (:mutator winner) mutation))))
                 (recur n accepted parent parent-obs traj))))))))))

;; ---------------------------------------------------------------------------
;; 3. Exhaustive search - the exponential baseline
;;
;; Enumerate programs shortest-first and keep the best seen. Chaitin: reaching
;; BB(N) this way takes ~2^N time, so the record should climb like log(trials).
;; ---------------------------------------------------------------------------

(defn flat-programs
  "Every flat program over `alphabet`, shortest first. Lazy and infinite.

  Flat: no nesting, so no multi-instruction :exec_y bodies. See the ns docstring
  on why that handicap is left in place."
  [alphabet]
  (mapcat (fn [k]
            (map (fn [xs] (apply list xs))
                 (combo/selections alphabet k)))
          (range 0 100)))

(defn exhaustive-search
  ([] (exhaustive-search {}))
  ([opts]
   (let [{:as opts :keys [trials seed]} (merge default-opts opts)]
     (binding [*rng* (random seed)]
       (loop [n 0
              [program & more] (flat-programs (:alphabet opts organism/alphabet))
              best (:start opts)
              best-obs (observe (:start opts) opts)
              traj [(record 0 (observe (:start opts) opts) (:start opts))]]
         (if-let [stopped (or (done? opts n best-obs)
                              (when (nil? program) :alphabet-exhausted))]
           (result :exhaustive-search opts n n best-obs best traj stopped)
           (let [obs (observe program opts)
                 n (inc n)]
             (if (organism/fitter? (:fitness obs) (:fitness best-obs))
               (recur n more program obs
                      ;; every enumerated program is a "trial" and a "step"
                      (conj traj (assoc (record n obs program) :accepted n)))
               (recur n more best best-obs traj)))))))))

;; ---------------------------------------------------------------------------

(defn compare-regimes
  "Runs all three regimes under one budget and returns their growth curves."
  ([] (compare-regimes {}))
  ([opts]
   (let [opts (merge default-opts opts)]
     (mapv (fn [f] (f opts))
           [exhaustive-search cumulative-evolution intelligent-design]))))

(defn summarise
  "One line per regime: how big a number did it learn to name, and how."
  [results]
  (mapv (fn [{:keys [regime trials accepted stopped best trajectory]}]
          {:regime regime
           :trials trials
           :accepted accepted
           :stopped stopped
           :records (count trajectory)
           :bits (:bits best)
           :saturated? (:saturated? best)
           :points (:points best)})
        results))

(defn cost-to-reach
  "What it cost the regime to first name a `bits`-bit number, or nil.

  This is the comparison Chaitin's theorem is about: hold the target fixed and
  read off the cost per regime. Two currencies, because they disagree and the
  disagreement is the interesting part:

    :trials   every mutation attempt / enumerated program - i.e. compute
    :accepted accepted mutations only - Chaitin's unit, in which his designer
              gets the best mutation for free"
  [{:keys [trajectory]} bits]
  (when-let [r (first (filter (fn [r] (<= bits (:bits r))) trajectory))]
    (select-keys r [:trials :accepted])))

(defn cost-table
  "Cost-to-reach each threshold, per regime. nil = never got there."
  [results thresholds currency]
  (mapv (fn [r]
          (into {:regime (:regime r)}
                (map (fn [b] [b (currency (cost-to-reach r b))]))
                thresholds))
        results))

(defn growth-curve
  "The record trajectory as [trials bits] pairs - the y axis is log2 fitness."
  [{:keys [trajectory]}]
  (mapv (juxt :trials :bits) trajectory))

;; ---------------------------------------------------------------------------
;; Replication
;;
;; Two of the three regimes are stochastic, so a single run says very little.
;; ---------------------------------------------------------------------------

(defn- median
  [xs]
  (let [xs (vec (sort xs))
        n (count xs)]
    (when (pos? n)
      (if (odd? n)
        (nth xs (quot n 2))
        (/ (+ (nth xs (dec (quot n 2))) (nth xs (quot n 2))) 2)))))

(defn replicate-comparison
  "Runs the stochastic regimes over `seeds` and exhaustive search once.

  Returns {:runs {regime [result ...]} :medians [...]}, where each median entry
  reports the median best-bits reached and, per regime, how many of the runs
  saturated the substrate. Exhaustive search is deterministic given the
  alphabet, so running it once is not a shortcut - it is the same run."
  ([] (replicate-comparison {}))
  ([{:as opts :keys [seeds] :or {seeds [1 2 3 4 5]}}]
   (let [opts (merge default-opts (dissoc opts :seeds))
         runs {:exhaustive-search [(exhaustive-search opts)]
               :cumulative-evolution
               (mapv (fn [s] (cumulative-evolution (assoc opts :seed s))) seeds)
               :intelligent-design
               (mapv (fn [s] (intelligent-design (assoc opts :seed s))) seeds)}]
     {:runs runs
      :medians
      (mapv (fn [[regime rs]]
              {:regime regime
               :runs (count rs)
               :median-bits (median (map (comp :bits :best) rs))
               :median-trials (median (map :trials rs))
               :median-accepted (median (map :accepted rs))
               :saturated (count (filter (comp :saturated? :best) rs))})
            runs)})))

;; ---------------------------------------------------------------------------
;; Frugal busy beaver: what does paying for resources do to the search?
;; ---------------------------------------------------------------------------

(defn compression-phase
  "The part of a run that happens *after* the organism first saturated.

  Only reachable under a frugal policy, where saturation is not terminal. Once
  the number cannot get bigger, every further accepted mutation is selection for
  naming the same number more cheaply - so this is a program-compression run
  wearing a busy-beaver costume, and the thing to watch is `:points` falling
  while `:bits` stays flat."
  [{:keys [trajectory]}]
  (when-let [from (first (keep-indexed (fn [i r] (when (:saturated? r) i))
                                       trajectory))]
    (let [phase (subvec trajectory from)]
      {:records (count phase)
       :bits (:bits (first phase))
       :points-from (:points (first phase))
       :points-to (:points (last phase))
       :steps-from (:steps (first phase))
       :steps-to (:steps (last phase))
       :curve (mapv (juxt :trials :points :steps) phase)})))

(defn compare-fitness-policies
  "Cumulative evolution under each fitness policy, over `seeds`.

  The axes reported are the same for every policy (bits / points / steps), so
  the comparison is about what selection *chose* to optimise, not about
  different yardsticks. Reports medians plus how many runs saturated and how
  many went on to compress."
  ([] (compare-fitness-policies {}))
  ([{:as opts :keys [policies seeds]
     :or {policies (vec (keys organism/fitness-policies))
          seeds [1 2 3 4 5]}}]
   (let [base (merge default-opts (dissoc opts :policies :seeds))
         runs (into {}
                    (map (fn [p]
                           [p (mapv (fn [s]
                                      (cumulative-evolution
                                       (-> base
                                           (assoc :seed s)
                                           (assoc-in [:organism :fitness] p))))
                                    seeds)]))
                    policies)]
     {:runs runs
      :medians
      (mapv (fn [[p rs]]
              (let [compressions (keep compression-phase rs)]
                {:fitness p
                 :runs (count rs)
                 :median-bits (median (map (comp :bits :best) rs))
                 :median-points (median (map (comp :points :best) rs))
                 :median-steps (median (map (comp :steps :best) rs))
                 :median-accepted (median (map :accepted rs))
                 :saturated (count (filter (comp :saturated? :best) rs))
                 :compressing (count (filter #(< 1 (:records %)) compressions))
                 :median-points-shed
                 (median (keep (fn [c] (when (< 1 (:records c))
                                         (- (:points-from c) (:points-to c))))
                               compressions))}))
            runs)})))

(comment
  (require '[benjamin-schwerdtner.metabiology.evolution :as evo] :reload)

  (def rs (evo/compare-regimes {:trials 5000}))
  (evo/summarise rs)
  (mapv evo/growth-curve rs))
