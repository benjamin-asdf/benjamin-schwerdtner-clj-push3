(ns benjamin-schwerdtner.problems.propeller-metabiology-test
  "Tests for putting Chaitin's three regimes on program synthesis, and for the
  two bugs in the shared metabiology code that doing so exposed."
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require
   [clojure.test :refer [deftest testing is]]
   [benjamin-schwerdtner.clj-push3-play.util :as util]
   [benjamin-schwerdtner.metabiology.evolution :as evolution]
   [benjamin-schwerdtner.metabiology.mutation :as mutation]
   [benjamin-schwerdtner.metabiology.organism :as organism]
   [benjamin-schwerdtner.problems.propeller.metabiology :as mb]
   [benjamin-schwerdtner.problems.propeller.problems :as p]
   [random-seed.core :as rs]))

;; ---------------------------------------------------------------------------
;; The alphabet leak
;;
;; :mb_rand_atom and :mb_rand_code closed over `organism/atom-generators`, so
;; every genome the regimes grew was built from BIGNUM instructions whatever
;; landscape they had been pointed at. Invisible in the busy-beaver runs, where
;; that IS the right alphabet, and total elsewhere: on a PSB problem the
;; inserted material could never do anything, so nothing was ever accepted.
;; ---------------------------------------------------------------------------

(defn- grown-children
  "Children strictly larger than the empty parent, over `n` random mutators."
  [alphabet n seed]
  (binding [rs/*rng* (rs/random seed)]
    (into []
          (comp (keep (fn [_]
                        (mutation/mutate '()
                                         (mutation/random-mutator {:alphabet alphabet})
                                         {:alphabet alphabet})))
                (filter #(< 1 (util/count-points %))))
          (range n))))

(deftest new-material-follows-the-alphabet-test
  (let [propeller (vec (p/alphabet (p/by-name :small-or-large)))
        from-propeller (grown-children propeller 1500 1)
        from-organism (grown-children organism/alphabet 1500 1)
        atoms (fn [programs] (set (flatten (map seq programs))))]
    (testing "growth happens at all"
      (is (seq from-propeller))
      (is (seq from-organism)))
    (testing "a propeller run never has BIGNUM instructions spliced into it"
      (is (empty? (filter #(and (keyword? %)
                                (clojure.string/starts-with? (name %) "bignum"))
                          (atoms from-propeller)))))
    (testing "while the busy-beaver alphabet still gets its own material,
              i.e. metabiology's own behaviour is unchanged"
      (is (seq (filter #(and (keyword? %)
                             (clojure.string/starts-with? (name %) "bignum"))
                       (atoms from-organism)))))
    (testing "and the two are actually different now - they were identical"
      (is (not= (atoms from-propeller) (atoms from-organism))))))

;; ---------------------------------------------------------------------------
;; The designer regime and non-numeric fitness
;;
;; intelligent-design picked its winner with `max-key`, which needs numbers.
;; The :parsimonious policies compare [score -points] vectors, so the one
;; regime most prone to bloat could not be run under the policies written to
;; control it.
;; ---------------------------------------------------------------------------

(deftest intelligent-design-accepts-comparable-fitness-test
  (testing "a vector-valued fitness policy no longer throws"
    (let [r (mb/run-regime (p/by-name :small-or-large) :intelligent-design
                           {:trials 200 :seed 1 :fitness :parsimonious
                            :n-cases 5 :n-train 10 :n-test 10})]
      (is (= :intelligent-design (:regime r)))
      (is (number? (:trials r)))))
  (testing "and the busy-beaver path still works, where max-key was fine"
    (let [r (binding [*err* (java.io.StringWriter.)]
              (evolution/intelligent-design
               {:trials 200 :seed 1
                :organism {:max-executions 50 :max-bignum-bits 500}}))]
      (is (= :intelligent-design (:regime r))))))

;; ---------------------------------------------------------------------------
;; Fitness policies
;; ---------------------------------------------------------------------------

(deftest fitness-policies-test
  (let [f #(:f (mb/policy %))]
    (testing ":solved maximises exact solves"
      (is (< ((f :solved) {:solved 3}) ((f :solved) {:solved 4}))))
    (testing ":neg-error prefers less error"
      (is (< ((f :neg-error) {:total-error 10.0})
             ((f :neg-error) {:total-error 2.0}))))
    (testing ":frugal-size separates equal solvers by size"
      (let [g (f :frugal-size)]
        (is (> (g {:solved 3 :points 10}) (g {:solved 3 :points 50})))))
    (testing ":frugal-size does NOT behave as it does on busy beavers.

              There the numerator is a tower of exponentials and dominates any
              program size, so every real gain is accepted whatever it costs.
              Here it is a bounded case count, so the DENOMINATOR dominates: a
              4-solver of 100 points loses to a 3-solver of 3 points. Pinned
              because the metabiology finding is stated in terms that quietly
              assume an unbounded numerator, and it does not carry over."
      (let [g (f :frugal-size)]
        (is (< (g {:solved 4 :points 100}) (g {:solved 3 :points 3})))))
    (testing "whereas :parsimonious keeps the intended meaning on a bounded
              score - more solved always wins, size only breaks ties"
      (let [g (f :parsimonious)]
        (is (pos? (compare (g {:solved 4 :points 100})
                           (g {:solved 3 :points 3}))))))
    (testing ":parsimonious compares lexicographically"
      (let [g (f :parsimonious)]
        (is (pos? (compare (g {:solved 4 :points 99}) (g {:solved 3 :points 1}))))
        (is (pos? (compare (g {:solved 3 :points 1}) (g {:solved 3 :points 9}))))))
    (testing "only :solved ends the run when it tops out - the others keep
              going, which is where frugality does its compression work"
      (is (true? (:saturation-terminal? (mb/policy :solved))))
      (is (false? (:saturation-terminal? (mb/policy :frugal-size)))))))

(deftest observe-test
  (let [problem (p/by-name :smallest)
        cases (:train (p/train-and-test problem 10 5 1))
        observe (mb/make-observe problem cases {:fitness :solved})
        solution '(:in1 :in2 :integer_min :in3 :integer_min
                   :in4 :integer_min :integer_print)]
    (testing "an empty program solves nothing and has not saturated"
      (let [o (observe '() {})]
        (is (zero? (:solved o)))
        (is (false? (:saturated? o)))))
    (testing "the progress axis :bits IS the solved count - the regime
              machinery reads :bits, and on this landscape that is cases"
      (let [o (observe solution {})]
        (is (= (:solved o) (:bits o)))
        (is (= (count cases) (:solved o)))))
    (testing "solving every case is saturation: nothing can be fitter, so the
              run is over - the analogue of hitting the bit budget"
      (is (true? (:saturated? (observe solution {})))))
    (testing "a correct program is fitter than nothing"
      (is (organism/fitter? (:fitness (observe solution {}))
                            (:fitness (observe '() {})))))))

;; ---------------------------------------------------------------------------
;; The regimes, end to end
;; ---------------------------------------------------------------------------

(deftest regimes-run-test
  (doseq [regime [:exhaustive-search :cumulative-evolution :intelligent-design]]
    (testing (str regime " produces a result on a benchmark problem")
      (let [r (mb/run-regime (p/by-name :small-or-large) regime
                             {:trials 400 :seed 1 :fitness :neg-error
                              :n-cases 8 :n-train 20 :n-test 20})]
        (is (= regime (:regime r)))
        (is (<= 0 (:train-solved r) (:n-train r)))
        (is (<= 0 (:test-solved r) (:n-test r)))
        (is (contains? #{:budget-exhausted :saturated :alphabet-exhausted}
                       (:stopped r)))))))

(deftest saturation-stops-the-run-test
  (testing "a solvable-by-accident problem stops early once every case is
            solved, rather than burning the rest of the budget"
    ;; integer-regression over a couple of cases is reachable by enumeration.
    (let [r (mb/run-regime (p/by-name :integer-regression) :exhaustive-search
                           {:trials 100000 :seed 1 :fitness :solved
                            :n-cases 1 :n-train 3 :n-test 3})]
      (is (contains? #{:saturated :budget-exhausted :alphabet-exhausted}
                     (:stopped r)))
      (when (= :saturated (:stopped r))
        (is (< (:trials r) 100000)
            "stopping on saturation means the budget was not exhausted")))))
