(ns benjamin-schwerdtner.metabiology.organism
  "Software organisms: a Push3 program is an animal.

  Chaitin's metabiology (Proving Darwin, 2012) in one breath:

  - Life is randomly evolving software. A single software organism, alone,
    is a mathematician.
  - The organism's job is to name the biggest integer it can. Fitness IS the
    Busy Beaver problem, so fitness is unbounded and there is no optimum -
    the process never ends. That is the point: evolution is creative, not
    mechanical.
  - Or fitness is the *frugal* busy beaver: the biggest number per unit of
    resource spent naming it. See [[fitness-policies]] - this is where the
    landscape stops rewarding bloat, and where a saturated run turns into a
    search for the shortest program naming a given number.
  - Mathematical creativity and biological creativity are the same thing,
    measured the same way.

  This namespace is the organism and its fitness. The variation operator lives
  in [[benjamin-schwerdtner.metabiology.mutation]] (algorithmic mutation) and the
  search regimes in [[benjamin-schwerdtner.metabiology.evolution]].

  ------------------------------------------------------------------------
  The oracle, and what we substitute for it
  ------------------------------------------------------------------------

  Chaitin's organisms are self-delimiting programs whose fitness is only
  defined if they halt, and he helps himself to a halting oracle to throw away
  the ones that don't. We do not have an oracle, so we bound execution steps
  instead and choose a reading policy:

  `:resource-bounded` (default) - read whatever number the organism has built
      when the step budget runs out. This turns the landscape into the
      *bounded* busy beaver: biggest number nameable in T steps. It is
      computable, which means the strict Chaitin theorem (fitness outgrows
      every computable function) does NOT hold of it. What survives is the
      part we can actually run: the comparison between search regimes, and
      the fact that raising T raises the ceiling forever.

  `:halting-required` - fitness 0 unless the organism halted inside the
      budget. Closer to Chaitin's letter, but it is brutal here: an organism
      that squares in an :exec_y loop is exactly the kind of good mathematician
      we want, and it never halts. Kept for comparison.

  The bit budget (`:max-bignum-bits`) is a second, separate bound - see
  benjamin-schwerdtner.clj-push3-play.instructions.bignum. An organism that
  saturates it has hit the wall of the substrate rather than the wall of its own
  cleverness, and [[observe]] reports that distinctly so it can never be
  mistaken for open-ended growth."
  (:require
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.stack.pushvalues :as pushvalues]))

(def zero java.math.BigInteger/ZERO)

(def default-opts
  {:max-executions 1000
   :max-bignum-bits (long 1e5)
   :oracle :resource-bounded
   :fitness :busy-beaver})

;; ---------------------------------------------------------------------------
;; Fitness policies - what counts as being a better mathematician
;;
;; T. Rado's busy beaver is "name the biggest number". The frugal variants ask
;; "name the biggest number *per unit of resource*", which is the natural answer
;; to the genome bloat that plain busy beaver produces (genomes doubling on a
;; neutral plateau because a tiny fitness gain pays for any amount of junk).
;;
;; The distinction that matters here, and it is easy to get wrong: when outputs
;; are towers, a RATIO of the number to a resource is almost a no-op. An
;; organism naming a 100000-bit number in 200 steps has log2 fitness 100000 -
;; 7.6. The penalty is 7.6 parts in 100000. So ratio frugality only bites at the
;; very bottom of the landscape, where it bites hard.
;;
;; Frugality in LOG space - bits per step, bits per code point - is
;; proportional, so it bites equally at every scale. That is the one that
;; actually controls bloat.
;;
;; `:saturation-terminal?` says whether hitting the substrate's bit budget ends
;; the search. Under plain busy beaver it does: nothing can be strictly fitter.
;; Under any frugal policy it does not, and this is the interesting part - a
;; saturated organism can still get fitter by naming the same number with less
;; program or fewer steps. Selection turns from "name a bigger number" into
;; "name this number more cheaply", which is a search for program-size
;; complexity.
;; ---------------------------------------------------------------------------

(def fitness-policies
  {:busy-beaver
   {:doc "The number named. Rado's busy beaver, and Chaitin's fitness."
    :saturation-terminal? true
    :f (fn [{:keys [named]}] (bigint named))}

   :frugal-steps
   {:doc "named / execution steps. The frugal busy beaver already sketched in
   problems/busy_beaver.clj. A ratio, so see the note above: negligible once
   outputs are towers."
    :saturation-terminal? false
    :f (fn [{:keys [named steps]}] (/ (bigint named) (max 1 steps)))}

   :frugal-size
   {:doc "named / program points. Ratio form of parsimony."
    :saturation-terminal? false
    :f (fn [{:keys [named points]}] (/ (bigint named) (max 1 points)))}

   :bits-per-step
   {:doc "log2(named) / execution steps. Frugality in log space: how much
   number do you name per step, proportional at every scale."
    :saturation-terminal? false
    :f (fn [{:keys [bits steps]}] (/ bits (max 1 steps)))}

   :bits-per-point
   {:doc "log2(named) / program points. The anti-bloat policy: junk in the
   genome costs proportionally, at every scale."
    :saturation-terminal? false
    :f (fn [{:keys [bits points]}] (/ bits (max 1 points)))}

   ;; -----------------------------------------------------------------------
   ;; Parsimony as a tie-breaker rather than a penalty.
   ;;
   ;; The dividing policies above turn out to freeze the search (measured, see
   ;; docs/metabiology.md): a resource penalty rejects every mutation that does
   ;; not immediately pay for itself, which kills the neutral drift plain busy
   ;; beaver relies on to find its stepping stones.
   ;;
   ;; These compare lexicographically instead: name a bigger number first, and
   ;; only *among equals* prefer the cheaper organism. Neutral drift survives,
   ;; because a lateral move at equal fitness and equal cost is still accepted;
   ;; what dies is paying for junk.
   ;; -----------------------------------------------------------------------

   :parsimonious
   {:doc "Maximise the number named; among equals prefer fewer program points."
    :saturation-terminal? false
    :f (fn [{:keys [named points]}] [(bigint named) (- points)])}

   :parsimonious-steps
   {:doc "Maximise the number named; among equals prefer fewer execution steps."
    :saturation-terminal? false
    :f (fn [{:keys [named steps]}] [(bigint named) (- steps)])}})

(defn policy
  [k]
  (or (get fitness-policies k)
      (throw (ex-info "Unknown fitness policy"
                      {:policy k :known (keys fitness-policies)}))))

;; ---------------------------------------------------------------------------
;; The alphabet
;;
;; Small on purpose. Chaitin's organisms live over a small alphabet, and a small
;; alphabet is what makes the exhaustive-search regime a meaningful baseline
;; rather than a joke. These are the rungs of the ladder
;; (N, N+N, N*N, N^N, ...) plus enough plumbing and recursion to build towers.
;; ---------------------------------------------------------------------------

(def alphabet
  (into (mapv (fn [i] i) (range 0 10))
        [:bignum_frominteger
         :bignum_+
         :bignum_*
         :bignum_-
         :bignum_succ
         :bignum_pow
         :bignum_dup
         :bignum_swap
         :bignum_max
         :bignum_bitlength
         :integer_+
         :integer_*
         :integer_dup
         :exec_y
         :exec_dup
         :exec_do*times]))

(def atom-generators
  (gen/identifier-generators alphabet))

(defn size
  "Program size, in Push code points. Our stand-in for |program| in bits.

  Chaitin measures organisms in bits of a self-delimiting binary encoding; we
  measure points of Push code. The unit differs by roughly a constant factor
  (log2 of the alphabet size), which is all the exponential weighting in
  [[benjamin-schwerdtner.metabiology.mutation]] actually cares about."
  [program]
  (util/count-points program))

(defn random-organism
  ([] (random-organism {}))
  ([{:keys [max-points] :or {max-points 12}}]
   (gen/generate-code {:max-points max-points
                       :atom-generators atom-generators
                       :branch-probability (constantly 0.15)
                       :close-probability (fn [n] (/ (- 1 (/ 1 n)) 3))})))

;; ---------------------------------------------------------------------------
;; Running an organism, and reading the number it names
;; ---------------------------------------------------------------------------

(defn- setup
  [{:keys [max-bignum-bits]}]
  (assoc-in (push/setup-state) [:parameters :max-bignum-bits] max-bignum-bits))

(defn- named-number
  "The number the organism names: the largest BIGNUM it left on its stack.

  Taking the max rather than the top of the stack is a deliberate kindness to
  random programs - an organism that builds something enormous and then pushes
  a 3 on top of it has still done the mathematics."
  [state]
  (let [stack (-> state :stacks :push/bignum)]
    (if (empty? stack)
      zero
      (reduce (fn [a b] (.max a b)) stack))))

(defn observe
  "Runs `program` as an organism and reports what it named.

  The reported axes are always the same regardless of `:fitness` policy, so runs
  under different policies stay comparable: the policy decides what selection
  optimises, `:bits` / `:steps` / `:points` are what we measure.

  Returns:
    :fitness     the selection value under the `:fitness` policy. Comparable,
                 but not necessarily a number: the frugal policies return ratios
                 and the parsimonious ones return [number cost] vectors that
                 compare lexicographically. Always compare with [[fitter?]].
    :named       BigInteger, the number the organism named (0 if none)
    :bits        bitLength of :named, i.e. log2 of the number named
    :points      program size in Push code points
    :halted?     did the EXEC stack empty before the step budget ran out
    :steps       execution steps actually used
    :saturated?  did :named hit the substrate's bit budget (see ns doc)"
  ([program] (observe program {}))
  ([program opts]
   (let [{:as opts :keys [max-executions oracle max-bignum-bits]}
         (merge default-opts opts)
         state (push/execute (setup opts)
                             program
                             {:max-executions max-executions})
         halted? (empty? (-> state :stacks :push/exec))
         built (named-number state)
         ;; The oracle rules on the organism before fitness is computed, so a
         ;; policy never gets to reward a program the oracle rejected.
         named (if (and (= oracle :halting-required) (not halted?))
                 zero
                 built)
         ctx {:named named
              :bits (.bitLength (.abs named))
              :points (size program)
              :steps (:push/num-executions state)}]
     (assoc ctx
            :fitness ((:f (policy (:fitness opts))) ctx)
            :halted? halted?
            :saturated? (<= max-bignum-bits (.bitLength (.abs built)))))))

(defn fitness
  "The selection value of `program` under the `:fitness` policy."
  ([program] (fitness program {}))
  ([program opts] (:fitness (observe program opts))))

(defn fitter?
  "Is `a` a strictly better mathematician than `b`?

  `compare` rather than .compareTo: fitness values are BigInt under
  `:busy-beaver` and Ratio under the frugal policies, and clojure's numeric
  comparison handles the mix."
  [a b]
  (pos? (compare a b)))

(comment
  (require '[benjamin-schwerdtner.metabiology.organism :as o] :reload)

  ;; A dull organism: names 7.
  (dissoc (o/observe '(7 :bignum_frominteger)) :named)
  ;; {:bits 3, :points 3, :steps 3, :fitness 7N, :halted? true, :saturated? false}

  ;; A good mathematician: squares in a loop, never halts, and saturates the
  ;; substrate rather than its own imagination.
  (dissoc (o/observe '(2 :bignum_frominteger :exec_y (:bignum_dup :bignum_*)))
          :named :fitness)
  ;; {:bits 100000, :points 7, :steps 1000, :halted? false, :saturated? true}

  ;; Same organism under Chaitin's letter: not halting means unfit.
  (:bits (o/observe '(2 :bignum_frominteger :exec_y (:bignum_dup :bignum_*))
                    {:oracle :halting-required}))
  ;; 0 - which is why we default to the other policy

  ;; The frugal busy beaver. Same organism, five verdicts. The ratio policies
  ;; stay tower-sized (don't coerce them to double, you get ##Inf) while the
  ;; log-space ones are small numbers you can read:
  (let [squarer '(2 :bignum_frominteger :exec_y (:bignum_dup :bignum_*))]
    (into (sorted-map)
          (for [p (keys o/fitness-policies)
                :let [v (o/fitness squarer {:fitness p})]]
            [p (if (ratio? v) (double v) (str (count (str v)) " digits"))])))
  ;; {:bits-per-point 14285.7, :bits-per-step 100.0,
  ;;  :busy-beaver "30103 digits", :frugal-size "30102 digits",
  ;;  :frugal-steps "30100 digits"}
  ;; - dividing a 30103-digit number by 1000 steps is not a penalty.

  ;; Bloat is only punished by the log-space policies. A genome padded with junk
  ;; that names the same number:
  (let [lean '(9 :bignum_frominteger 9 :bignum_pow)
        bloated (concat lean (repeat 40 :bignum_stackdepth))]
    (for [p [:busy-beaver :frugal-size :bits-per-point]]
      [p (o/fitter? (o/fitness lean {:fitness p})
                    (o/fitness (apply list bloated) {:fitness p}))]))

  (o/size '(2 :bignum_frominteger :exec_y (:bignum_dup :bignum_*)))
  (repeatedly 5 o/random-organism))
