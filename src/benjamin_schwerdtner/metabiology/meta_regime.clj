(ns benjamin-schwerdtner.metabiology.meta-regime
  "A fourth regime: meta evolution.

  Chaitin's three regimes all draw their mutation from a fixed distribution -
  exhaustive enumeration, cumulative evolution's random algorithmic mutation,
  intelligent design's oracle over a pool of draws. In every one of them, M is
  SAMPLED.

  This regime makes M a population under selection instead
  (clj_push3_play/mutation/dual.clj, following ideas/metaevolution.org), and
  asks the same question on the same axis: what does it cost to first name an
  N-bit number.

  Results are shaped like [[benjamin-schwerdtner.metabiology.evolution]]'s, so
  they drop straight into its `cost-table` / `growth-curve` / `cost-to-reach`.

  ---------------------------------------------------------------------------
  Three conditions, because one comparison would prove the wrong thing
  ---------------------------------------------------------------------------

  Running evolved-M-with-a-population against Chaitin's single-organism walk
  would conflate meta evolution with population parallelism, and the new
  regime would win for a reason that has nothing to do with the claim. So the
  conditions differ in exactly one thing at a time, all on the same loop:

    :sampled   M drawn fresh from the 2^-|M| algorithmic distribution
               (metabiology.mutation/random-mutator), picked uniformly per
               event, never selected. This is cumulative evolution's variation
               operator, run inside the same population machinery.
    :library   the hand-written operator set, still picked uniformly and never
               selected. Isolates what the OPERATORS are worth, separately
               from any selection on them.
    :meta      the same library, now under selection and reproducing.
               Isolates what META EVOLUTION adds on top of the library.

  `:m-tournament 1` is what makes the first two unselected - a tournament of
  one is a uniform draw. That is the whole difference, so it cannot be
  confounded by anything else in the loop.

  ---------------------------------------------------------------------------
  Units
  ---------------------------------------------------------------------------

  `:n-variations 1`, so one mutation event produces exactly one child and
  `:trials` here means the same thing it means in evolution.clj: one draw of a
  mutation, one candidate. Without that a single event could emit three
  children and the axis would silently stretch.

  The trajectory is recorded per GENERATION, not per record-holder, so a cost
  read off it is rounded up to the end of the generation in which the record
  fell. That bias runs against this regime, which is the safe direction for a
  regime whose author would like it to win."
  (:require
   [benjamin-schwerdtner.metabiology.organism :as organism]
   [benjamin-schwerdtner.metabiology.mutation :as mutation]
   [benjamin-schwerdtner.clj-push3-play.mutation.dual :as dual]))

(def default-opts
  {:generations 40
   :seed 1
   ;; Matches evolution.clj's *defaults* - NOT the runs written up in
   ;; docs/metabiology.md, which used 60,000 trials and a 1e5 bit budget.
   ;; Against those numbers this regime is running a 20x smaller trial budget
   ;; against a 10x lower ceiling, so its costs are NOT on the same axis and
   ;; must not be read next to that table. Comparing means re-running all four
   ;; under one setting, in one fresh JVM - see the doc's own warning about a
   ;; long-lived nREPL image being a hidden variable.
   :organism {:max-executions 200
              :max-bignum-bits 10000
              :oracle :resource-bounded}
   :e-size 20
   :events-per-generation 50
   :m-size 26})

(defn- bits
  [fitness]
  (if fitness (.bitLength (biginteger fitness)) 0))

(defn- to-result
  "Reshape a dual-evolution history into an evolution.clj-style result."
  [regime opts history]
  (let [traj (:traj
              (reduce
               (fn [{:keys [trials accepted traj]} h]
                 (let [trials (+ trials (or (:events h) 0))
                       accepted (+ accepted (or (:improvements h) 0))]
                   {:trials trials
                    :accepted accepted
                    :traj (conj traj
                                {:trials trials
                                 :accepted accepted
                                 :bits (bits (:best-fitness h))
                                 :generation (:generation h)
                                 :saturated?
                                 (<= (get-in opts [:organism :max-bignum-bits])
                                     (bits (:best-fitness h)))
                                 :program (:best-program h)})}))
               {:trials 0 :accepted 0 :traj []}
               history))
        best (reduce (fn [a b] (if (< (:bits a) (:bits b)) b a))
                     {:bits -1}
                     traj)]
    {:regime regime
     :trials (:trials (last traj) 0)
     :accepted (:accepted (last traj) 0)
     :stopped (if (:saturated? best) :saturated :budget-exhausted)
     :best best
     :trajectory traj
     :opts (select-keys opts [:seed :generations :organism :e-size
                              :events-per-generation :m-size])}))

(def conditions
  "How each condition configures the shared loop. See the ns docstring.

  `:meta` differs from `:library` in TWO ways - events are steered by
  selection, and the operator population turns over. `:selected` sits between
  them with selection but no turnover, so the two effects can be told apart.
  Without it a loss by `:meta` could equally well mean `selection suppresses
  the operator that matters` or `reproduction replaced the good operators with
  random code`, and those call for opposite fixes."
  {:sampled  {:m-tournament 1 :m-replace-frac 0}
   :library  {:m-tournament 1 :m-replace-frac 0}
   :selected {:m-tournament 3 :m-replace-frac 0 :credit-mode :lineage}
   :meta     {:m-tournament 3 :m-replace-frac 0.3 :credit-mode :lineage}})

(defn- m-seeds
  [condition m-size]
  (if (= condition :sampled)
    ;; Chaitin's variation operator: drawn from the algorithmic distribution,
    ;; where the probability of a mutation is exponential in its program size.
    (vec (repeatedly m-size mutation/random-mutator))
    dual/seed-mutators))

(defn run
  "Run one condition. `condition` is a key of [[conditions]]."
  ([condition] (run condition {}))
  ([condition opts]
   (let [{:as opts :keys [generations seed organism e-size
                          events-per-generation m-size]}
         (merge default-opts opts)
         seeds (m-seeds condition m-size)
         history
         (:history
          (binding [*err* (java.io.StringWriter.)]
            (dual/run generations
                      (merge {:fitness #(organism/fitness % organism)
                              :random-genome organism/random-organism
                              :alphabet organism/alphabet
                              ;; Bound mutators by the same BIGNUM budget the
                              ;; organisms get. On config defaults a :code_run
                              ;; probe gets 10x the bit budget of the thing it
                              ;; is probing and costs more than a real fitness
                              ;; evaluation.
                              :parameters (select-keys organism
                                                       [:max-bignum-bits])
                              :e-size e-size
                              :events-per-generation events-per-generation
                              ;; One event, one child - see ns docstring.
                              :n-variations 1
                              :m-seeds seeds
                              :m-size (max m-size (count seeds))
                              :seed seed}
                             (conditions condition)
                             ;; Anything else the caller passed goes through to
                             ;; the loop. Without this, opts like :cost-mode
                             ;; were silently swallowed here and the run used
                             ;; defaults while appearing to test something.
                             (dissoc opts :generations :organism)))))]
     (to-result condition opts history))))

(defn compare-conditions
  "All three conditions on the same seed, ready for evolution.clj's cost-table."
  ([] (compare-conditions {}))
  ([opts]
   (mapv #(run % opts) [:sampled :library :selected :meta])))

(comment
  (require '[benjamin-schwerdtner.metabiology.evolution :as evo])

  (def rs (compare-conditions {:seed 1 :generations 30}))

  (mapv (juxt :regime #(-> % :best :bits) :trials :stopped) rs)
  (evo/cost-table rs [16 64 256 1024] :trials)
  (evo/growth-curve (first rs)))
