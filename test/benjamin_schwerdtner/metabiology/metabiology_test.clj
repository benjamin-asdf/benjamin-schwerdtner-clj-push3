(ns benjamin-schwerdtner.metabiology.metabiology-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.metabiology.organism :as organism]
   [benjamin-schwerdtner.metabiology.mutation :as mutation]
   [benjamin-schwerdtner.metabiology.evolution :as evolution]))

(defn- bignum-stack
  [program & [{:keys [max-executions] :or {max-executions 500}}]]
  (-> (push/execute (push/setup-state) program {:max-executions max-executions})
      :stacks
      :push/bignum))

(deftest bignum-stack-test
  (testing "unbounded arithmetic, which INTEGER's 1e10 clamp cannot express"
    (is (= [(biginteger 81)] (bignum-stack '(3 :bignum_frominteger 4 :bignum_pow))))
    (is (= [(biginteger 10)] (bignum-stack '(5 :bignum_frominteger :bignum_dup :bignum_+))))
    (is (= [(biginteger 8)] (bignum-stack '(7 :bignum_frominteger 8 :bignum_frominteger :bignum_max)))))

  (testing "a BIGNUM literal lands on the BIGNUM stack, not clj-object"
    (is (= :push/bignum (push/typeof-item (biginteger 5))))
    ;; clojure's BigInt stays an INTEGER, and stays clamped
    (is (= :push/integer (push/typeof-item (bigint 5)))))

  (testing "exponentiation is guarded before it allocates"
    ;; 2^1e9 would be a 1e9-bit number; it must saturate, not eat the heap
    (let [[v] (bignum-stack '(2 :bignum_frominteger 1000000000 :bignum_pow))]
      (is (= 100000 (.bitLength v))))
    (is (= [java.math.BigInteger/ZERO]
           (bignum-stack '(0 :bignum_frominteger 999 :bignum_pow))))
    (is (= [(biginteger -8)] (bignum-stack '(-2 :bignum_frominteger 3 :bignum_pow))))
    (testing "even powers of -1 are positive"
      (is (= [java.math.BigInteger/ONE]
             (bignum-stack '(-1 :bignum_frominteger 1000000000 :bignum_pow)))))
    (testing "anything to the zero is one"
      (is (= [java.math.BigInteger/ONE]
             (bignum-stack '(9 :bignum_frominteger 0 :bignum_pow))))))

  (testing "the bit budget is configurable per run"
    (let [state (assoc-in (push/setup-state) [:parameters :max-bignum-bits] 64)
          v (peek (-> (push/execute state
                                    '(2 :bignum_frominteger :exec_y (:bignum_dup :bignum_*))
                                    {:max-executions 200})
                      :stacks
                      :push/bignum))]
      (is (= 64 (.bitLength v))))))

(deftest random-instructions-are-actually-random-test
  (testing "*rng* advances across execution steps within a run"
    ;; Before the fix in interpreter/execute-1 these three draws were identical,
    ;; because the RNG was re-seeded on every step.
    (let [draws (fn [seed]
                  (-> (push/execute (assoc (push/setup-state)
                                           :push/random-seed seed)
                                    '(:integer_rand :integer_rand :integer_rand
                                                    :integer_rand :integer_rand)
                                    {:max-executions 500})
                      :stacks
                      :push/integer))]
      (testing "five draws in one run are not all the same value"
        ;; the old behaviour: every step re-seeded, so all five were identical
        (is (= 5 (count (draws 0))))
        (is (< 1 (count (distinct (draws 0))))))
      (testing "and a run stays reproducible from its seed"
        (is (= (draws 0) (draws 0)))
        (is (not= (draws 0) (draws 42)))))))

(deftest organism-test
  (testing "fitness is the number named"
    (let [obs (organism/observe '(7 :bignum_frominteger))]
      (is (= (biginteger 7) (:fitness obs)))
      (is (true? (:halted? obs)))
      (is (false? (:saturated? obs)))))

  (testing "an organism that names nothing has fitness zero, not an error"
    (is (= organism/zero (organism/fitness '())))
    (is (= organism/zero (organism/fitness '(:bignum_pop :bignum_*)))))

  (testing "the number named is the largest left on the stack"
    ;; building something big and then pushing a 3 on top is still mathematics
    (is (= (biginteger 100) (organism/fitness '(100 :bignum_frominteger
                                                    3 :bignum_frominteger)))))

  (testing "the two oracle policies disagree about a non-halting organism"
    (let [squarer '(2 :bignum_frominteger :exec_y (:bignum_dup :bignum_*))]
      (is (false? (:halted? (organism/observe squarer))))
      (is (organism/fitter? (organism/fitness squarer) (biginteger 1e9)))
      ;; under Chaitin's letter, not halting means unfit - which kills exactly
      ;; the organisms we most want
      (is (= organism/zero
             (organism/fitness squarer {:oracle :halting-required}))))))

(deftest fitness-policy-test
  (let [lean '(9 :bignum_frominteger 9 :bignum_pow)
        ;; names exactly the same number, with 40 points of dead weight
        bloated (apply list (concat lean (repeat 40 :bignum_stackdepth)))
        bigger '(9 :bignum_frominteger 9 :bignum_pow 9 :bignum_pow)
        fit (fn [p prog] (organism/fitness prog {:fitness p}))]

    (testing "the measured axes are identical across policies, so runs compare"
      (let [axes (fn [p] (-> (organism/observe lean {:fitness p})
                             (select-keys [:named :bits :points :steps])))]
        (is (apply = (map axes (keys organism/fitness-policies))))))

    (testing "plain busy beaver is indifferent to bloat"
      (is (= (fit :busy-beaver lean) (fit :busy-beaver bloated)))
      (is (not (organism/fitter? (fit :busy-beaver lean)
                                 (fit :busy-beaver bloated)))))

    (testing "every frugal and parsimonious policy punishes bloat"
      (doseq [p [:frugal-steps :frugal-size :bits-per-step :bits-per-point
                 :parsimonious :parsimonious-steps]]
        (is (organism/fitter? (fit p lean) (fit p bloated))
            (str p " should prefer the lean organism"))))

    (testing "parsimony is a tie-breaker, not a penalty: it never blocks growth"
      ;; `bigger` names a far bigger number and costs more. A penalty policy may
      ;; refuse it; a lexicographic one must not.
      (doseq [p [:busy-beaver :parsimonious :parsimonious-steps]]
        (is (organism/fitter? (fit p bigger) (fit p lean))
            (str p " should prefer the bigger number"))))

    (testing "fitter? handles ratio and vector fitness, not just integers"
      (is (ratio? (fit :bits-per-point lean)))
      (is (vector? (fit :parsimonious lean)))
      (is (false? (organism/fitter? (fit :parsimonious lean)
                                    (fit :parsimonious lean)))))

    (testing "saturation only ends the search when nothing could be fitter"
      (is (true? (:saturation-terminal? (organism/policy :busy-beaver))))
      (doseq [p [:frugal-steps :bits-per-point :parsimonious]]
        (is (false? (:saturation-terminal? (organism/policy p))))))

    (testing "an unknown policy fails loudly"
      (is (thrown? clojure.lang.ExceptionInfo (organism/policy :no-such-policy))))))

(deftest algorithmic-mutation-test
  (testing "doubling the genome is one algorithmic step"
    ;; Hamming distance maximal, algorithmic distance 1. This is the headline.
    (is (= '(2 :bignum_frominteger 2 :bignum_frominteger)
           (mutation/mutate '(2 :bignum_frominteger) '(:mut_duplicate)))))

  (testing "reversing the genome is one algorithmic step"
    (is (= '(5 4 3 2 1) (mutation/mutate '(1 2 3 4 5) '(9 0 :mut_inverse)))))

  (testing "a mutator that touches nothing is a neutral mutation"
    (is (= '(1 2 3) (mutation/mutate '(1 2 3) '()))))

  (testing "children that bloat past the size bound are refused"
    (is (nil? (mutation/mutate '(1 2 3) '(:mut_duplicate)
                               {:max-organism-points 2}))))

  (testing "novel material comes from the organism alphabet, not gensym junk"
    (let [alphabet (set organism/alphabet)
          material (mapcat flatten
                           (repeatedly 40 #(mutation/variants '() '(9 :mb_rand_code))))]
      (is (seq material))
      (is (every? alphabet material))))

  (testing "the mutation distribution decays exponentially in program size"
    (let [sizes (repeatedly 400 #(mutation/sample-size mutation/default-opts))
          small (count (filter #(<= % 6) sizes))
          large (count (filter #(< 12 %) sizes))]
      (is (< large small))))

  (testing "mutation distance is an upper bound on relative information content"
    (let [d (mutation/mutation-distance '(2 :bignum_frominteger)
                                        '(2 :bignum_frominteger 2 :bignum_frominteger)
                                        {:max-points 4 :tries-per-size 400})]
      (is (some? d) "genome doubling should be findable at tiny mutator size")
      (is (<= (:points d) 4)))))

(deftest regimes-test
  (testing "every regime reports the same shape and honest stop reason"
    (let [opts {:trials 400
                :organism {:max-executions 100
                           :max-bignum-bits 512
                           :oracle :resource-bounded}}]
      (doseq [r (evolution/compare-regimes opts)]
        (is (keyword? (:regime r)))
        (is (contains? #{:budget-exhausted :saturated :alphabet-exhausted}
                       (:stopped r)))
        (is (<= 1 (count (:trajectory r))))
        (testing "trajectories are monotone in fitness - they are record-holders"
          (is (apply <= (map :bits (:trajectory r)))))
        (testing "cost-to-reach is nil for a target that was never reached"
          (is (nil? (evolution/cost-to-reach r 1000000)))))))

  (testing "cumulative evolution outruns exhaustive search"
    ;; The Chaitin separation: 2^N vs N^2. Given a budget where exhaustive
    ;; search cannot leave the single-bit range, the random walk should.
    (let [opts {:trials 12000
                :seed 1
                :organism {:max-executions 200
                           :max-bignum-bits 10000
                           :oracle :resource-bounded}}
          ex (evolution/exhaustive-search opts)
          ce (evolution/cumulative-evolution opts)]
      (is (< (:bits (:best ex)) (:bits (:best ce))))))

  (testing "saturation ends a busy-beaver run but never a frugal one"
    (let [opts {:trials 8000 :seed 1
                :organism {:max-executions 200 :max-bignum-bits 128}}
          bb (evolution/cumulative-evolution
              (assoc-in opts [:organism :fitness] :busy-beaver))
          par (evolution/cumulative-evolution
               (assoc-in opts [:organism :fitness] :parsimonious))]
      (when (:saturated? (:best bb))
        (is (= :saturated (:stopped bb))
            "nothing can be fitter than a saturated organism under plain BB"))
      ;; under parsimony a saturated organism can still shed points, so the walk
      ;; must never be cut short by saturation
      (is (not= :saturated (:stopped par)))))

  (testing "runs are reproducible from their seed"
    (let [opts {:trials 2000 :seed 3
                :organism {:max-executions 100 :max-bignum-bits 1000
                           :oracle :resource-bounded}}]
      (is (= (evolution/growth-curve (evolution/cumulative-evolution opts))
             (evolution/growth-curve (evolution/cumulative-evolution opts)))))))
