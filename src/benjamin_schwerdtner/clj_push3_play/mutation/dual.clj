(ns benjamin-schwerdtner.clj-push3-play.mutation.dual
  "Dual evolution: the loop of ideas/metaevolution.org.

  Two populations, not one.

  - `E`, the domain genomes, evolving against a fitness landscape.
  - `M`, the mutation operators, Push programs that take an E genome and emit
    variations of it. `mutate(E, M)`.

  E is selected on fitness. M is selected on *evolvability*: an M earns its
  place by producing offspring that beat their parent. That is a lineage-level
  criterion - it is not about how fit the mutator itself is, it is about the
  rate of improvement it induces in somebody else - which is the shape of the
  selection in Barnett/Meister/Rainey 2025, where success depended on the
  capacity to evolve between phenotypic states.

  Three things the document asks for and this loop provides:

  1. MUTATION IS EVOLVED, not fixed. M is Push code, so the space of mutation
     operators is the space of programs: `:mut_duplicate` doubles a genome in
     one step, which point mutation reaches only with probability 2^-k. This
     is the algorithmic-mutation point (Chaitin 1975): mutation distance is
     program size complexity, not Hamming distance.

  2. EVOLVABILITY IS DYNAMIC (third order). Markers ride on the program tree
     as Clojure metadata - age, stagnation, whether the host is starving, plus
     whatever M itself wrote - and M reads them with :code_meta_get. A mutator
     that mutates hard when its host is stagnating and gently when it is
     improving is an adaptation *in the domain of evolvability*, and here it
     is a reachable genotype rather than a hand-written schedule.

  3. THE MARKERS ARE WRITABLE (the leap of faith). M can :code_meta_set, and
     what it writes is on the tree the child inherits. Signals from the
     mutation operator travel transgenerationally. Because a marker is
     attached to a subtree rather than naming it, it survives the region being
     moved - and duplication hands it to both copies, which is the substrate
     gene duplication needs.

  What this loop does NOT claim: `:self` meta-mutation below makes M mutate M,
  which is autoconstructive in Spector's sense, but there is no ruminative
  layer - nothing here has representations *about* evolutionary paths. That is
  ideas/popperian-meta-evolution.org and it is not implemented."
  (:require
   [benjamin-schwerdtner.clj-push3-play.mutation.mut :as mut]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [random-seed.core :as rs :refer [rand rand-int rand-nth]])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;; ---------------------------------------------------------------------------
;; The alphabet M evolves over
;;
;; Note this is a DIFFERENT alphabet from the one E evolves over. M's domain is
;; program transformation, so its instructions are the CODE and MUT ones plus
;; the epigenetic register. E's alphabet belongs to whatever problem is being
;; solved and is handed in as `:alphabet`.
;; ---------------------------------------------------------------------------

(def mutator-instructions
  [;; variation proper
   :mut_duplicate :mut_rotate :mut_inverse :mut_interleave
   :mut_drop_rand :mut_drop_rand_one
   ;; structural surgery
   :code_dup :code_swap :code_pop :code_car :code_cdr :code_cons :code_append
   :code_nth :code_nthcdr :code_subst :code_insert :code_length :code_list
   :code_rand_atom :code_noop
   ;; reading and writing markers on the tree - third order
   :code_meta_get :code_meta_set :code_meta_set_int :code_meta_inc
   ;; reading other genomes - this is what turns the operators above into
   ;; sexual ones, since :mut_interleave and friends then have a partner
   :code_other :code_other_nth :code_elder
   ;; the interpreter, as an ordinary primitive. :code_rand_atom can draw it
   ;; like anything else, so a mutator can EVOLVE to evaluate its candidates
   ;; rather than judge them by their shape. Sexual selection on performance
   ;; instead of on ornament.
   :code_run
   ;; enough float plumbing to do something with what :code_run returns
   :float_= :float_< :float_> :float_- :float_dup :float_pop
   ;; enough control flow to be conditional on a signal
   :exec_if :exec_dup :exec_when])

(def mutator-atom-generators
  (into (gen/identifier-generators mutator-instructions)
        [;; indices and counts for rotate / nth / inverse
         (fn [] (rand-int 8))
         ;; rates for the stochastic operators
         (fn [] (rand))
         ;; keys the mutator can use as markers. `:stagnation`, `:age` and
         ;; `:starving?` are the ones this loop writes, so a mutator that
         ;; draws one of them can condition on it; `:mut-count` and
         ;; `:conserved` are the ones the bookkeeping seeds use.
         (fn [] (rand-nth [:stagnation :age :starving?
                           :mut-count :conserved]))]))

(defn random-mutator
  ([] (random-mutator 12))
  ([max-points]
   (gen/generate-code {:max-points max-points
                       :atom-generators mutator-atom-generators
                       :branch-probability (constantly 0.15)
                       :close-probability (fn [n] (/ (- 1 (/ 1 n)) 3))})))

(def seed-mutators
  "Hand-written mutators to start M off.

  Spector's Pushpop had to prop up early generations because random programs
  had not yet evolved to reproduce at all. Here the null mutator already
  clones (the genome starts on the CODE stack), so this is not about
  competence - it is about not spending the first hundred generations
  rediscovering `drop one instruction`. Everything here is reachable from
  random code; seeding just skips the boring part."
  ['()                                        ;; clone - the null mutator
   '(:mut_drop_rand_one)                      ;; deletion
   '(:mut_duplicate)                          ;; whole genome doubling
   '(0.15 :mut_drop_rand)                     ;; deletion, rate-based
   '(1 :mut_rotate)                           ;; reading frame shift
   '(:code_dup :code_rand_atom :code_append)  ;; insertion
   '(:code_dup :code_cdr)                     ;; truncation, keeps the original
   '(:code_dup :mut_interleave)               ;; self-recombination
   '(:code_dup :mut_drop_rand_one :mut_interleave)

   ;; --- third order, by hand ---------------------------------------------
   ;; Drastic when stagnating (duplicate), mild otherwise (drop one).
   ;; (:exec_if runs its SECOND exec item when the condition is true, and
   ;; `a b :integer_>` compares a > b.)
   '(:stagnation :code_meta_get 2 :integer_> :exec_if
                 (:mut_drop_rand_one)
                 (:mut_duplicate))

   ;; --- bookkeeping ------------------------------------------------------
   ;; These are seeded rather than left to be discovered, and that is a
   ;; deliberate concession: there is no gradient toward bookkeeping. A
   ;; mutator that only increments a counter pays for it and gains nothing,
   ;; because the payoff arrives generations later when something READS the
   ;; counter. Parts that are useless alone do not evolve from scratch. So
   ;; the experiment is not "does it arise" but "is it retained once
   ;; present", which is also what Barnett/Rainey actually demonstrated.
   ;;
   ;; Note the ORDER, which is not incidental. `(:mut-count :code_meta_inc
   ;; :mut_drop_rand_one)` looks right and does nothing: the tally is written,
   ;; and then the mutation rebuilds the list and throws the metadata away.
   ;; Modifying a region destroys its markers - only MOVING preserves them.
   ;; So a mutator that wants a running tally has to read it before, carry it
   ;; on the INTEGER stack across the mutation, and write it back after. It
   ;; has to actively rescue its own bookkeeping. That is a property of the
   ;; genotype, not of the framework, which is the point.
   '(:mut-count :code_meta_get
                :mut_drop_rand_one
                1 :integer_+ :mut-count :code_meta_set_int)
   ;; The same, but the tally is READ to decide: gentle once this genome has
   ;; been worked over a lot, drastic while it is still fresh. Only here does
   ;; the bookkeeping pay for itself.
   ;; Both branches mutate deliberately. An earlier version had the high-tally
   ;; branch do nothing, which made the seed suicidal: with no marker yet the
   ;; leftover threshold ends up in the counter, every later read is above it,
   ;; and the mutator clones forever.
   '(:mut-count :code_meta_get :integer_dup
                4 :integer_> :exec_if
                (:mut_duplicate)
                (:mut_drop_rand_one)
                1 :integer_+ :mut-count :code_meta_set_int)
   ;; --- sexual -----------------------------------------------------------
   ;; The same recombination machinery as above, but with a partner instead
   ;; of the genome itself. `(:code_dup :mut_interleave)` was the strongest
   ;; operator in the asexual runs and it was recombining a genome with a
   ;; copy of itself - sex without a partner.
   '(:code_other :mut_interleave)              ;; crossover, interleaved
   '(:code_other :code_append)                 ;; concatenation, symbiosis
   '(:code_other :mut_drop_rand_one :mut_interleave)
   ;; Recombine with the dead. A module that was fine can outlive the
   ;; organism that died for some unrelated reason.
   '(:code_elder :mut_interleave)

   ;; Mate choice on executed behaviour, not on ornament: run the candidate
   ;; on two inputs and refuse it if its output does not move - a constant is
   ;; a bad partner whatever the target happens to be. Uses the interpreter
   ;; as an ordinary primitive (:code_run), so this is sexual selection on
   ;; performance rather than on the shape of the code.
   ;;
   ;; The rejection branch REDRAWS rather than giving up, and that detail is
   ;; the whole operator. The give-up version - reject, then clone - loses to
   ;; having no filter at all, because a childless event still consumes a
   ;; reproductive opportunity and can never earn credit. Being choosy has to
   ;; mean taking the next partner, not skipping the season. Measured over 4
   ;; seeds: redraw 9.7% improvement rate, no filter 7.9%, give-up 9.1% but
   ;; starved of events.
   ;;
   ;; Honest caveat, since this is seeded on the strength of that number: the
   ;; win is on M's CREDIT METRIC. On the held-out benchmark it was one clear
   ;; win and two near-ties over three seeds, i.e. no reliable effect. In the
   ;; one run where it did break out of the plateau it did so with HALF the
   ;; improvement events of the control - which is the standing warning that
   ;; `improved?` counts steps and not distance.
   '(:code_other 0.0 0.0 1.0 :code_run 2.0 :code_run :float_=
                 :exec_if (:mut_interleave) (:code_pop :code_other :mut_interleave))

   ;; Duplicate, then mark the result conserved: the Ohno move. Both copies
   ;; carry whatever the parent subtree carried, so the marker is what tells
   ;; a later mutator which copy is free to drift.
   ;; (Value first, key on top - and the value goes through
   ;; :code_meta_set_int because `true` would land on BOOLEAN, not
   ;; CLJ-OBJECT, and the instruction would starve and NOOP.)
   '(:mut_duplicate 1 :conserved :code_meta_set_int)])

;; ---------------------------------------------------------------------------
;; Populations
;; ---------------------------------------------------------------------------

(defn- e-individual
  [program]
  {:program program :fitness nil :age 0 :stagnation 0 :ancestry {}})

(defn- m-individual
  [program]
  ;; All three are decaying counters, not totals: an M that was useful fifty
  ;; generations ago should not coast on it, because which mutation operator
  ;; is useful changes as E moves through the landscape. That drift is the
  ;; whole point of evolving M.
  {:program program :offspring 0.0 :improvements 0.0 :lineage 0.0
   :cost 0.0 :age 0})

(def prior-rate
  "What we assume an untried mutator's improvement rate is, before evidence.

  This has to sit near the realistic rate (a few percent), NOT at 0.5. With a
  Laplace 1/2 prior an untried mutator outranks one that tried ten times and
  improved three - so every operator we had actually learned something about
  got culled and the population churned untried code forever. The prior is
  mild optimism, enough that a new operator gets a turn, not so much that
  ignorance beats a track record."
  0.1)

(def prior-strength
  "How many observations the prior is worth."
  2.0)

(defn improvement-rate
  "How often this mutator's offspring beat their parent, smoothed toward
  [[prior-rate]].

  The smoothing matters in both directions: without it an M with one lucky
  offspring scores 1.0 and takes over the population on a sample of one."
  [{:keys [offspring improvements]}]
  (/ (+ improvements (* prior-strength prior-rate))
     (+ offspring prior-strength)))

;; ---------------------------------------------------------------------------
;; Lineage credit
;;
;; [[improvement-rate]] asks: did the child beat its parent, right now. That is
;; a boolean per event, and it rewards an operator that produces many tiny
;; wins over one that rarely makes a structural jump - we measured exactly
;; that, a run whose held-out benchmark broke out of its plateau while
;; producing HALF the improvement events of its control. Cadence, not
;; distance.
;;
;; Evolvability does not claim the next step is better. It claims the LINEAGE
;; gets further. So credit an M by how much of the surviving population
;; descends from its interventions, over a window of generations.
;;
;; Every E individual carries `:ancestry`, a normalised distribution over the
;; mutators responsible for it. A child inherits its parent's ancestry, faded
;; by `ancestry-decay`, plus a full share for the operator that just acted.
;; Each generation the survivors pay that share back out. An operator that
;; made a genome which keeps founding survivors keeps earning, generations
;; after the step itself - which is the point, and is what a single-step
;; measure structurally cannot see.
;;
;; This is the lineage-level selection of Barnett/Meister/Rainey 2025, where
;; success depended on the capacity to evolve rather than on being fit.
;; ---------------------------------------------------------------------------

(def ancestry-floor
  "Shares below this are dropped, so ancestry maps stay small instead of
  accumulating a long tail of ancestors with negligible responsibility."
  0.02)

(defn- inherit-ancestry
  "The child's ancestry: the parent's, faded, plus a share for `mutator`.

  Normalised to sum to 1, so one surviving individual always pays out exactly
  one unit of credit however deep its pedigree. Without that, credit would
  scale with how long a lineage happens to have been running and older
  operators would drown out newer ones for free."
  [parent-ancestry mutator decay]
  (let [faded (into {}
                    (keep (fn [[k v]]
                            (let [v (* decay v)]
                              (when (<= ancestry-floor v) [k v]))))
                    parent-ancestry)
        raw (update faded mutator (fnil + 0.0) 1.0)
        total (reduce + 0.0 (vals raw))]
    (if (pos? total)
      (into {} (map (fn [[k v]] [k (/ v total)])) raw)
      {mutator 1.0})))

(defn lineage-rate
  "Share of the surviving population descending from this mutator, per event
  it was given. Smoothed like [[improvement-rate]] and on the same scale, so
  the two are interchangeable as a selection criterion."
  [{:keys [offspring lineage]}]
  (/ (+ lineage (* prior-strength prior-rate))
     (+ offspring prior-strength)))

;; ---------------------------------------------------------------------------
;; Frugality
;;
;; M is allowed to be arbitrarily powerful. It may read the population, run
;; candidates in a sandbox, keep books in metadata - in principle implement a
;; whole evolutionary algorithm inside itself. The discipline does not come
;; from forbidding any of that. It comes from the bill.
;;
;; So the criterion splits in two:
;;
;;   numerator   what M achieved   (`credit-mode`: :step or :lineage)
;;   denominator what M consumed   (`cost-mode`:   :events or :resources)
;;
;; `:events` is the old behaviour - every mutation event costs the same,
;; whatever it did. `:resources` prices the two things that were free and
;; should not have been:
;;
;;   - M's own execution steps, including everything it ran in a :code_run
;;     sandbox. A stack machine hands you this exactly: one EXEC pop is one
;;     unit, uniformly, at every level of nesting.
;;   - the fitness evaluations M demanded. Emitting three variants instead of
;;     one buys three chances at an improvement; under :events that is free,
;;     which makes `emit as many children as possible` a strictly dominant
;;     strategy for reasons that have nothing to do with being a good mutator.
;;
;; This is the frugal busy beaver moved up a level: not `how big a number`,
;; but `how much improvement per unit of compute`.
;; ---------------------------------------------------------------------------

(defn event-cost
  "Cost of one mutation event, in units where a typical event is about 1."
  [{:keys [steps n-children]} {:keys [step-cost-unit offspring-cost]}]
  (+ (/ (double (or steps 0)) (double step-cost-unit))
     (* (double offspring-cost) (double (or n-children 0)))))

(defn credit-score
  "The criterion M is selected on.

  `credit-mode` picks the numerator, `cost-mode` the denominator. Both
  alternatives are kept runnable, because each is the control for the other -
  `:step`/`:events` together are the original behaviour."
  [{:keys [credit-mode cost-mode]}]
  (let [numerator (case credit-mode
                    :lineage :lineage
                    :step :improvements)]
    (fn [m]
      (/ (+ (get m numerator 0.0) (* prior-strength prior-rate))
         (+ (case cost-mode
              :resources (:cost m 0.0)
              :events (:offspring m 0.0))
            prior-strength)))))

(defn- distinct-by
  "Transducer that keeps the first item for each `(f item)`."
  [f]
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc x]
         (let [k (f x)]
           (if (contains? @seen k)
             acc
             (do (vswap! seen conj k) (rf acc x)))))))))

(defn- tournament
  "Pick the best of `size` random members, by `score` under `better?`."
  [population score better? size]
  (let [picks (repeatedly (max 1 size) #(rand-nth population))]
    (reduce (fn [a b] (if (better? (score b) (score a)) b a)) picks)))

;; ---------------------------------------------------------------------------
;; One generation
;; ---------------------------------------------------------------------------

(defn- evaluate
  "Fitness of `program`, memoized - the same genome recurs constantly across a
  run and fitness is usually the expensive part."
  [cache fitness program]
  (if-let [[_ f] (find @cache program)]
    f
    (let [f (fitness program)]
      (swap! cache assoc program f)
      f)))

(defn- signals
  "What the loop knows about this individual, merged into its genome's
  metadata before M runs.

  Markers M wrote in an earlier generation are already on the tree and survive
  this - [[mut/with-signals]] merges rather than replaces. That is the
  transgenerational channel: `mut` wrote it, the child inherits it, because it
  is on the code object the child is built from."
  [{:keys [age stagnation]}]
  {:age age
   :stagnation stagnation
   :starving? (< 2 stagnation)})

(defn- breed
  "One mutation event: `parent` mutated by `m`. Returns the children (already
  evaluated) and whether the best of them beat the parent."
  [parent m {:keys [fitness better? cache n-variations alphabet max-executions
                    population graveyard ancestry-decay parameters
                    e-max-points]}
   seed]
  (let [{:keys [variations steps]}
        (mut/variations (:program parent)
                        (:program m)
                        {:seed seed
                         :epigenetics (signals parent)
                         :alphabet alphabet
                         :population population
                         :graveyard graveyard
                         :parameters parameters
                         :n-variations n-variations
                         :max-executions max-executions})
        children
        (for [v variations
              ;; A mutator that returns the parent unchanged has cloned, which
              ;; is legal but is not an improvement and must not earn credit.
              ;;
              ;; NOTE there is deliberately no check against the rest of the
              ;; population here. Now that M can read other genomes, "return
              ;; the fittest individual you can find" is a strategy that beats
              ;; most parents and earns credit without producing anything new.
              ;; Forbidding it would be a hand-written rule; the intended
              ;; answer is frequency-dependent selection, which makes copying
              ;; unprofitable rather than illegal. Watch for it - the parasite
              ;; pressure acts on a slower timescale than this credit window.
              ;;
              ;; And a size cap, because nothing else bounds E. `mut_duplicate`
              ;; doubles a genome in one step - that is the point of
              ;; algorithmic mutation - so ANY operator that can duplicate will
              ;; run away once selection finds it, and selection finds it fast:
              ;; measured, genomes went 23 -> 3601 -> 81088 -> 230755 points
              ;; over 37 generations, and a single generation went from 28ms to
              ;; 3.5 minutes. Oversized children are dropped rather than
              ;; truncated - truncating would silently hand the operator a
              ;; different mutation than the one it made, and it would still
              ;; earn credit for it.
              :when (and (not= v (:program parent))
                         (or (nil? e-max-points)
                             (<= (util/count-points v) e-max-points)))]
          (let [f (evaluate cache fitness v)]
            {:program v
             :fitness f
             :age 0
             ;; Who is responsible for this genome existing, faded over the
             ;; ancestors. This is what the survivors pay back out.
             :ancestry (inherit-ancestry (:ancestry parent)
                                         (:program m)
                                         ancestry-decay)
             :stagnation (if (better? f (:fitness parent))
                           0
                           (inc (:stagnation parent)))}))]
    {:children children
     ;; What this event consumed, in the one currency a stack machine gives
     ;; you for free: EXEC pops. `steps` already includes anything M ran in a
     ;; :code_run sandbox, because the sandbox charges its steps back to its
     ;; caller - so nested evaluation is priced in the same unit as the
     ;; mutator's own work, at every level.
     :steps (or steps 0)
     :n-children (count children)
     :improved? (boolean (some #(better? (:fitness %) (:fitness parent))
                               children))}))

(defn step
  "One generation of dual evolution."
  [{:keys [e m generation seed cache graveyard] :as state}
   {:keys [e-size m-size e-tournament m-tournament credit-decay
           m-replace-frac m-mutator m-max-points better? fitness
           events-per-generation before-generation stationary-fitness?
           graveyard-size credit-mode]
    :as config}]
  (binding [rs/*rng* (rs/random (+ (* 1000003 seed) generation))]
    ;; Hook for a non-stationary landscape: the caller advances whatever E is
    ;; being measured against - a parasite population, a shifting target -
    ;; before anything is evaluated this generation.
    (when before-generation (before-generation state))
    ;; A moving landscape invalidates every memoized fitness. Keeping the
    ;; cache across a generation whose target changed would silently score
    ;; this generation's genomes against last generation's world.
    (when-not stationary-fitness? (reset! cache {}))
    (let [score (credit-score config)
          e (mapv (fn [ind]
                    (assoc ind :fitness (evaluate cache fitness (:program ind))))
                  e)
          ;; The population M may read is frozen here, before any offspring
          ;; exist. Otherwise events later in the generation would see the
          ;; results of earlier ones and the run would stop being reproducible.
          population (mapv :program e)
          ;; --- mutation events ---------------------------------------------
          ;; Deliberately more events than E has slots. M's credit signal is
          ;; far sparser than E's: E gets a fitness number for every genome,
          ;; while M only learns something when an offspring actually beats
          ;; its parent, which is a small fraction of events. Running extra
          ;; events per generation is the cheapest way to give selection on
          ;; evolvability something to work with.
          events
          (doall
           (for [i (range (or events-per-generation e-size))]
             (let [parent (tournament e :fitness better? e-tournament)
                   mutator (tournament m score > m-tournament)
                   ;; A distinct seed per event is what makes two offspring of
                   ;; the same parent under the same mutator differ. The
                   ;; SplitMix scramble in random-seed.core is what makes
                   ;; these counter-derived seeds actually independent.
                   ;; `cache` lives in the state, not the config; breed reads
                   ;; it out of the map it is handed.
                   result (breed parent mutator
                                 (assoc config
                                        :cache cache
                                        :population population
                                        :graveyard graveyard)
                                 (+ (* 7919 generation) i))]
               (assoc result :mutator (:program mutator) :parent parent))))

          ;; --- E: non random survival of the variations --------------------
          pool (concat (map #(update % :age inc) e)
                       (mapcat :children events))
          ;; A three-way comparator, not (if (better? a b) -1 1): equal
          ;; fitnesses are the common case here and a comparator that never
          ;; reports equality violates the sort contract.
          fittest-first (fn [a b]
                          (cond (better? a b) -1
                                (better? b a) 1
                                :else 0))
          ;; Distinct genomes only. Without this the population fills with
          ;; copies of the current best within a few generations, every parent
          ;; picked is the same program, and both E and M stop moving. Pushpop
          ;; disallowed identical programs for exactly this reason
          ;; (ideas/autoconstructive.org, "Diversity management").
          ranked-pool (sort-by :fitness fittest-first pool)
          next-e (into []
                       (comp (distinct-by :program) (take e-size))
                       ranked-pool)

          ;; --- the graveyard -----------------------------------------------
          ;; Genomes that did not make the cut, kept around and readable with
          ;; :code_elder. "Licences me to keep genomes of dead individuals
          ;; around, as part of the algorithm" (ideas/metaevolution.org). An
          ;; organism dies for one reason while most of its modules were fine;
          ;; this is the channel by which those modules can outlive it.
          survivors-set (into #{} (map :program) next-e)
          next-graveyard
          (into [] (take (or graveyard-size 200))
                (concat (into [] (comp (map :program)
                                       (remove survivors-set)
                                       (distinct))
                              ranked-pool)
                        graveyard))

          ;; --- M: credit ---------------------------------------------------
          ;; Two ledgers, kept side by side so either can drive selection and
          ;; the other stays readable as a comparison.
          ;;
          ;; `improvements` is per event and immediate: did this child beat
          ;; its parent. `lineage` is per SURVIVOR and retrospective: every
          ;; individual still standing pays out one unit, split over the
          ;; operators in its ancestry. An operator earns from it in every
          ;; generation its descendants persist, long after the step itself -
          ;; which is what makes it a measure of evolvability rather than of
          ;; step quality.
          credited
          (reduce (fn [acc {:as event :keys [mutator improved?]}]
                    (-> acc
                        (update-in [mutator :offspring] (fnil inc 0.0))
                        (update-in [mutator :improvements]
                                   (fnil + 0.0) (if improved? 1.0 0.0))
                        ;; The bill for this event: M's own steps plus the
                        ;; evaluations it demanded. See [[credit-score]].
                        (update-in [mutator :cost]
                                   (fnil + 0.0) (event-cost event config))))
                  {}
                  events)
          credited
          (reduce (fn [acc ind]
                    (reduce-kv (fn [acc mutator share]
                                 (update-in acc [mutator :lineage]
                                            (fnil + 0.0) share))
                               acc
                               (:ancestry ind)))
                  credited
                  next-e)
          m (mapv (fn [ind]
                    (let [{:keys [offspring improvements lineage cost]}
                          (get credited (:program ind) {})]
                      (-> ind
                          (update :age inc)
                          (update :offspring #(+ (* credit-decay %) (or offspring 0.0)))
                          (update :improvements
                                  #(+ (* credit-decay %) (or improvements 0.0)))
                          (update :lineage
                                  #(+ (* credit-decay %) (or lineage 0.0)))
                          (update :cost
                                  #(+ (* credit-decay %) (or cost 0.0))))))
                  m)

          ;; --- M reproduction ----------------------------------------------
          ;; `m-replace-frac` 0 freezes M: the population still competes for
          ;; events on its track record, but no new operators appear. That is
          ;; the control condition - it is what tells you whether an
          ;; improvement came from evolving the mutators or just from running
          ;; a bag of them.
          n-replace (if (pos? m-replace-frac)
                      (max 1 (long (* m-replace-frac m-size)))
                      0)
          ranked-m (vec (sort-by score > m))
          survivors (vec (drop-last n-replace ranked-m))
          offspring
          (doall
           (for [i (range n-replace)]
             (let [parent (tournament ranked-m score > m-tournament)
                   ;; `:self` is the autoconstructive setting: M is varied by
                   ;; a member of M, so the mutation operators are responsible
                   ;; for their own variation too. Anything else is a fixed
                   ;; bootstrap operator, which is the safer default - a
                   ;; population of clone-only mutators cannot vary itself out
                   ;; of that state.
                   meta-m (if (= m-mutator :self)
                            (:program (rand-nth ranked-m))
                            m-mutator)
                   vs (mut/mutate (:program parent) meta-m
                                  {:seed (+ (* 104729 generation) i)
                                   :alphabet mutator-instructions
                                   :n-variations 1})
                   child (first vs)]
               (m-individual
                (if (and child (<= (util/count-points child) m-max-points))
                  child
                  ;; Meta-mutation failed or ran away; inject fresh code
                  ;; rather than another copy of the parent, so M keeps
                  ;; exploring instead of converging on one operator.
                  (random-mutator m-max-points))))))]
      (assoc state
             :e next-e
             :m (into survivors offspring)
             :graveyard next-graveyard
             :generation (inc generation)
             :events events))))

;; ---------------------------------------------------------------------------
;; Driving it
;; ---------------------------------------------------------------------------

(def defaults
  {:e-size 40
   ;; Comfortably above (count seed-mutators), or the seeded operators fill
   ;; the whole population and M has no random code left to explore with.
   :m-size 26
   :n-variations 3
   ;; Mild parent selection on purpose. A greedy tournament makes every parent
   ;; the current champion, improving on it is rare, and then M gets almost no
   ;; evidence at all - the meta level starves before the domain level does.
   :e-tournament 2
   :m-tournament 3
   :events-per-generation 120
   ;; How fast an M's track record fades. Slow, because improvements are rare
   ;; enough that a fast decay erases the evidence before it accumulates.
   :credit-decay 0.95
   ;; What M is selected on. `:lineage` credits an operator by how much of the
   ;; surviving population descends from it; `:step` is the original per-event
   ;; "did the child beat its parent", kept as the control.
   :credit-mode :lineage
   ;; What M is charged for. `:resources` prices execution steps and demanded
   ;; evaluations; `:events` charges one flat unit per event and is the
   ;; control. M may be arbitrarily powerful - read the population, run
   ;; candidates, keep books - but under `:resources` it pays for all of it,
   ;; and the discipline comes from the bill rather than from a prohibition.
   :cost-mode :resources
   ;; EXEC pops per cost unit. Set to the cost of ONE FITNESS EVALUATION, so
   ;; that a unit means the same thing on both terms and the bill is
   ;; proportional to real compute.
   ;;
   ;; The consequence is worth stating, because it looks like a miscalibration
   ;; and is not: a plain mutator runs ~10 pops against an organism's ~200, so
   ;; its step term is ~0.05 and the evaluation term dominates. There is
   ;; genuinely almost nothing to save on a one-line operator. Measured, every
   ;; simple operator bills ~1.02-1.12 per event while the one that runs
   ;; :code_run probes bills 2.46. The brake engages exactly where M starts
   ;; spending - which is the point.
   :step-cost-unit 200
   ;; What one fitness evaluation costs, in the same units. Non-zero on
   ;; purpose: emitting three variants buys three chances at an improvement,
   ;; and if that is free then `emit as many children as possible` dominates
   ;; for reasons unrelated to being a good mutator.
   :offspring-cost 1.0
   ;; How fast responsibility for a genome fades back through its ancestors.
   ;; 0.7 puts the useful window at roughly five generations - long enough for
   ;; a structural change to prove itself, short enough that an operator
   ;; cannot live forever on one ancient success.
   :ancestry-decay 0.7
   :m-replace-frac 0.3
   :m-mutator '(:code_dup :code_rand_atom :code_append)
   :m-max-points 24
   ;; Size cap on the DOMAIN genomes. There is no other bound anywhere:
   ;; `sanitize-value :push/code` in stack/pushvalues.clj is still an
   ;; unimplemented TODO ("check max code points") and the config's
   ;; `:max-points-in-program` is read by nothing. nil disables the cap.
   :e-max-points 500
   :max-executions (long 1e4)
   :better? (fn [a b] (pos? (compare a b)))
   ;; Set false for a coevolutionary / moving landscape; the fitness cache is
   ;; then dropped every generation. See `before-generation`.
   :stationary-fitness? true
   :graveyard-size 200
   :seed 0})

(defn initial-state
  [{:keys [e-size m-size m-max-points seed random-genome fitness m-seeds]
    :or {m-seeds seed-mutators}
    :as config}]
  (binding [rs/*rng* (rs/random seed)]
    (let [cache (atom {})]
      {:generation 0
       :seed seed
       :cache cache
       :graveyard []
       ;; Evaluated here so that generation 0 of the history is the starting
       ;; line rather than a row of nils.
       :e (vec (repeatedly e-size
                           #(let [p (random-genome)]
                              (assoc (e-individual p)
                                     :fitness (evaluate cache fitness p)))))
       :m (vec (concat (map m-individual m-seeds)
                       (repeatedly (max 0 (- m-size (count m-seeds)))
                                   #(m-individual (random-mutator m-max-points)))))})))

(defn snapshot
  "What a generation looked like, for the history."
  [{:keys [e m generation events graveyard]}
   {:as config :keys [better? benchmark]}]
  (let [best (reduce (fn [a b] (if (better? (:fitness b) (:fitness a)) b a)) e)
        score (credit-score config)
        top-m (first (sort-by score > m))]
    {:generation generation
     :best-fitness (:fitness best)
     :best-program (:program best)
     ;; Under coevolution `:best-fitness` is RELATIVE - it is scored against a
     ;; world that moves, so comparing it across generations means nothing.
     ;; `benchmark` is the objective yardstick, held out of selection and used
     ;; only for reporting, which is the only way to tell real progress from
     ;; the opponents having got easier.
     ;;
     ;; It is taken over the WHOLE population, not over `best`. The
     ;; relatively-best individual is not the objectively best one once the
     ;; landscape moves, so scoring only that one reports the population's
     ;; churn as if it were the population's competence.
     :benchmark (when benchmark (reduce min (map (comp benchmark :program) e)))
     :best-mutator (:program top-m)
     ;; Both ledgers, whichever is driving selection. If they disagree that is
     ;; information: it says the operator that founds surviving lineages is
     ;; not the one that wins the most individual steps.
     :best-mutator-rate (score top-m)
     :best-mutator-step (improvement-rate top-m)
     :best-mutator-lineage (lineage-rate top-m)
     :m-distinct (count (distinct (map :program m)))
     :graveyard (count graveyard)
     ;; The density of M's credit signal. If `:improvements` sits at zero,
     ;; selection on evolvability is running on noise and any story about the
     ;; mutators having adapted is unfounded - so it is reported, not hidden.
     :events (count events)
     :children (reduce + 0 (map (comp count :children) events))
     :improvements (count (filter :improved? events))}))

(defn run
  "Run dual evolution.

  Required config:
    `fitness`        program -> a comparable fitness
    `random-genome`  () -> a random E genome
    `alphabet`       the domain's instructions, for :code_rand_atom in M

  Everything else defaults, see [[defaults]]. Returns the final state with a
  `:history` of per-generation snapshots."
  [n-generations config]
  (let [config (merge defaults config)]
    (loop [state (initial-state config)
           history []]
      (let [history (conj history (snapshot state config))]
        (if (<= n-generations (:generation state))
          (assoc state :history history)
          (recur (step state config) history))))))

(comment
  ;; A landscape to try it on: name the largest integer you can, on the plain
  ;; INTEGER stack. Small, fast, self-contained.
  (require '[benjamin-schwerdtner.clj-push3-play.interpreter :as push]
           '[benjamin-schwerdtner.clj-push3-play.generator :as gen])

  (def alphabet
    (into (vec (range 0 10))
          [:integer_+ :integer_* :integer_- :integer_dup :integer_swap
           :exec_y :exec_dup :exec_do*times]))

  (defn fit [program]
    (try
      (let [st (push/execute (assoc (push/setup-state)
                                    :exceptions-are-warnings? true)
                             program {:max-executions 300})
            stack (-> st :stacks :push/integer)]
        (if (empty? stack) 0 (long (apply max stack))))
      (catch Throwable _ 0)))

  (def r
    (binding [*err* (java.io.StringWriter.)]
      (run 25 {:fitness fit
               :alphabet alphabet
               :random-genome
               #(gen/generate-code
                 {:max-points 10
                  :atom-generators (gen/identifier-generators alphabet)
                  :branch-probability (constantly 0.15)
                  :close-probability (fn [n] (/ (- 1 (/ 1 n)) 3))})
               :e-size 30 :m-size 20 :seed 1})))

  (mapv (juxt :generation :best-fitness :children :improvements) (:history r))
  ;; 64 -> 96 -> 158 -> 316 -> 632 -> 1264 -> 2528 -> 5056 -> 10112, and
  ;; :improvements runs 9, 2, 8, 20, 52, 99, 120, 120 - the credit signal for M
  ;; is densest exactly while E is climbing, then goes to zero on the plateau.

  ;; ---------------------------------------------------------------------
  ;; The control, and what it says
  ;; ---------------------------------------------------------------------
  ;; `:m-replace-frac 0` freezes M. Against a single fixed operator
  ;; (:m-seeds ['(:code_dup :code_rand_atom :code_append)] :m-size 1) and
  ;; against the frozen seed bag, over seeds 1/2/3, evolving M did NOT
  ;; reliably win: the run-to-run spread is larger than any difference between
  ;; the conditions, and several runs hit 1e10, which is the INTEGER clamp
  ;; rather than a result.
  ;;
  ;; So: this landscape cannot answer whether meta evolution pays. It
  ;; saturates, and it is stationary - and a STATIONARY landscape is precisely
  ;; the case where evolvability has nothing to buy you. Barnett/Meister/
  ;; Rainey got selection for evolvability out of an environment that KEPT
  ;; CHANGING, where the winning lineages were the ones able to switch between
  ;; phenotypic states. Testing this loop needs a landscape with that shape.
  )
