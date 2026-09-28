(ns benjamin-schwerdtner.problems.propeller-test
  "Tests for the Propeller port.

  The tests that matter most here are the ones pinning REFERENCE SOLUTIONS to
  externally known values. A wrong reference solution does not fail loudly -
  it produces a benchmark that looks like the published one, is quietly a
  different problem, and makes every number measured against it meaningless.
  So `fuel-cost` is checked against the Advent of Code 2019 day 1 examples it
  is taken from, `bowling` against the three games every bowler knows the
  score of, and so on."
  (:require
   [clojure.test :refer [deftest testing is]]
   [benjamin-schwerdtner.problems.propeller.alphabet :as alphabet]
   [benjamin-schwerdtner.problems.propeller.data :as data]
   [benjamin-schwerdtner.problems.propeller.gp :as gp]
   [benjamin-schwerdtner.problems.propeller.problems :as p]
   [benjamin-schwerdtner.problems.propeller.run :as run]))

;; ---------------------------------------------------------------------------
;; Propeller's instruction inventory
;;
;; Transcribed from propeller_problems/push/instructions/: the `generate-
;; instructions` calls (polymorphic x 11 types, numeric x 2, vector x 4,
;; _print x 11) plus every `def-instruction`. Used by alphabet-test as the
;; completeness check on the port.
;; ---------------------------------------------------------------------------

(def ^:private polymorphic-suffixes
  ["_dup" "_dup_times" "_dup_items" "_empty" "_eq" "_flush" "_pop" "_rot"
   "_shove" "_stack_depth" "_swap" "_yank" "_yank_dup" "_deep_dup"])

(def ^:private polymorphic-types
  ["boolean" "char" "code" "exec" "float" "integer" "string"
   "vector_boolean" "vector_float" "vector_integer" "vector_string"])

(def ^:private numeric-suffixes
  ["_gt" "_gte" "_lt" "_lte" "_add" "_subtract" "_mult" "_quot" "_mod"
   "_max" "_min" "_inc" "_dec" "_from_boolean" "_from_char" "_from_string"])

(def ^:private vector-suffixes
  ["_butlast" "_concat" "_conj" "_contains" "_emptyvector" "_first" "_indexof"
   "_iterate" "_last" "_length" "_nth" "_occurrencesof" "_pushall" "_remove"
   "_replace" "_replacefirst" "_rest" "_reverse" "_set" "_subvec" "_take"])

(def propeller-instructions
  (mapv keyword
        (concat
         (for [t polymorphic-types s polymorphic-suffixes] (str t s))
         (for [t ["integer" "float"] s numeric-suffixes] (str t s))
         ["float_sin" "float_cos" "float_tan" "float_div" "float_from_integer"
          "integer_from_float"]
         ["boolean_and" "boolean_or" "boolean_not" "boolean_xor"
          "boolean_invert_first_then_and" "boolean_invert_second_then_and"
          "boolean_from_float" "boolean_from_integer"]
         ["char_all_from_string" "char_from_float" "char_from_integer"
          "char_is_digit" "char_is_letter" "char_is_whitespace"]
         ["code_append" "exec_do_count" "exec_do_range" "exec_do_times"
          "exec_do_while" "exec_if" "exec_k" "exec_s" "exec_when" "exec_while"
          "exec_y"]
         ["string_butlast" "string_concat" "string_conj_char" "string_contains"
          "string_contains_char" "string_drop" "string_empty_string"
          "string_first" "string_from_boolean" "string_from_char"
          "string_from_float" "string_from_integer" "string_indexof_char"
          "string_iterate" "string_last" "string_length" "string_nth"
          "string_occurrencesof_char" "string_parse_to_chars"
          "string_remove_char" "string_replace" "string_replace_char"
          "string_replace_first" "string_replace_first_char" "string_rest"
          "string_reverse" "string_set_char" "string_split" "string_substr"
          "string_take"]
         (for [t polymorphic-types] (str t "_print"))
         ["print_newline"]
         ;; Defined inside the multiplier problem files in Propeller rather
         ;; than in its instruction set; ordinary instructions here. `bufa`
         ;; and `nota` are deliberately absent - they exist in mul3.cljc but
         ;; are commented out of its alphabet, so no problem can draw them.
         ["boolean_nand" "boolean_nor" "boolean_xnor"]
         (for [i (range 8)] (str "set-c" i))
         (for [i (range 8)] (str "c" i))
         (for [t ["vector_boolean" "vector_float" "vector_integer"
                  "vector_string"]
               s vector-suffixes]
           (str t s)))))

;; ---------------------------------------------------------------------------
;; Reference solutions against known values
;; ---------------------------------------------------------------------------

(deftest reference-solutions-test
  (testing "fuel-cost, against the Advent of Code 2019 day 1 examples"
    (is (= 2 (p/fuel-cost [12])))
    (is (= 2 (p/fuel-cost [14])))
    (is (= 654 (p/fuel-cost [1969])))
    (is (= 33583 (p/fuel-cost [100756])))
    (is (= 34241 (p/fuel-cost [12 14 1969 100756]))))

  (testing "bowling, against games with known scores"
    (is (= 300 (p/bowling "XXXXXXXXXXXX")) "a perfect game")
    (is (= 0 (p/bowling "--------------------")) "all gutter balls")
    (is (= 90 (p/bowling "9-9-9-9-9-9-9-9-9-9-")) "nine and a miss each frame")
    (is (= 150 (p/bowling "5/5/5/5/5/5/5/5/5/5/5")) "a spare every frame"))

  (testing "luhn"
    ;; Eight doubled ones give two each, eight undoubled give one each.
    (is (= 24 (p/luhn (vec (repeat 16 1)))))
    ;; Nine doubled is 18, which casts out to 9 - so every position scores 9.
    (is (= 144 (p/luhn (vec (repeat 16 9)))))
    (testing "the doubling starts at index 1, not index 0"
      (is (= 3 (p/luhn [1 1])) "second digit doubled: 1 + 2")
      (is (= 1 (p/luhn [1 0])) "doubling a zero is still zero: 1 + 0")))

  (testing "small-or-large has a DEAD ZONE, it is not a two-way split"
    (is (= "small" (p/small-or-large 999)))
    (is (= "" (p/small-or-large 1000)))
    (is (= "" (p/small-or-large 1500)))
    (is (= "" (p/small-or-large 1999)))
    (is (= "large" (p/small-or-large 2000)))
    (is (= "large" (p/small-or-large 2001))))

  (testing "fizz-buzz"
    (is (= "FizzBuzz" (p/fizz-buzz 15)))
    (is (= "Fizz" (p/fizz-buzz 9)))
    (is (= "Buzz" (p/fizz-buzz 25)))
    (is (= "7" (p/fizz-buzz 7))))

  (testing "dice-game"
    (is (= 0.0 (p/dice-game 1 1)))
    (is (= 0.5 (p/dice-game 2 1)))
    ;; Fifteen of the thirty-six outcomes have Peter strictly ahead.
    (is (< (abs (- (/ 15.0 36.0) (p/dice-game 6 6))) 1e-9)))

  (testing "solve-boolean evaluates left to right, with no precedence"
    (is (true? (p/solve-boolean "t")))
    (is (false? (p/solve-boolean "f")))
    (is (false? (p/solve-boolean "t&f")))
    (is (true? (p/solve-boolean "t|f")))
    (is (true? (p/solve-boolean "f|f|t")))
    (is (true? (p/solve-boolean "t&t&t")))
    ;; No precedence is the point: with precedence this would be true.
    (is (false? (p/solve-boolean "f&t|f"))))

  (testing "the rest"
    (is (= 1 (p/basement [1 -2])))
    (is (= 0 (p/basement [-1])))
    (is (= 3 (p/basement [1 1 1 -100])))
    (is (= "b" (p/middle-character "abc")))
    (is (= "bc" (p/middle-character "abcd")))
    (is (= "helloWorld" (p/camel-case "hello-world")))
    (is (= "aBC" (p/camel-case "a-b-c")))
    (is (= "149" (p/square-digits 123)))
    (is (= "81" (p/square-digits 9)))
    (is (= 3 (p/paired-digits "1111")))
    (is (= 0 (p/paired-digits "12")))
    (is (= "olleh dlrow" (p/spin-words "hello world")))
    (is (= "abcd" (p/spin-words "abcd")) "under five letters is left alone")
    (is (= 1 (p/scrabble-score "a")))
    (is (= 10 (p/scrabble-score "q")))
    (is (= 0 (p/scrabble-score "!")) "non-letters score nothing")
    (is (= "B" (p/grade 90 80 70 60 85)))
    (is (= "A" (p/grade 90 80 70 60 90)))
    (is (= "F" (p/grade 90 80 70 60 0)))
    (is (= [1 2] (p/find-pair [3 1 2] 3)))
    (is (= "You didn't type anything" (p/twitter "")))
    (is (= "your tweet has 2 characters" (p/twitter "ab")))
    (is (= 4 (p/gcd 12 8)))
    (is (= 6 (p/count-odds [1 3 5 7 9 11 2 4])))))

(deftest multiplier-reference-test
  (testing "mul3, as bits MSB first"
    (is (= [true true false false false true]
           (p/mul3 true true true true true true)) "7 x 7 = 49 = 110001")
    (is (= [false false false false false true]
           (p/mul3 false false true false false true)) "1 x 1 = 1")
    (is (= [false false false true false false]
           (p/mul3 false true false false true false)) "2 x 2 = 4")
    (is (= [false false false false false false]
           (p/mul3 false false false true true true)) "0 x 7 = 0"))
  (testing "mul4"
    (is (= [true true true false false false false true]
           (p/mul4 true true true true true true true true))
        "15 x 15 = 225 = 11100001")
    (is (= 8 (count (p/mul4 false false false true false false false true))))))

;; ---------------------------------------------------------------------------
;; Epsilon lexicase
;; ---------------------------------------------------------------------------

(deftest epsilon-lexicase-test
  (testing "median and median absolute deviation"
    (is (= 2 (gp/median [1 2 3])))
    (is (= 2.5 (gp/median [1 2 3 4])))
    (is (= 5 (gp/median [5])))
    (is (nil? (gp/median [])))
    (is (= 0 (gp/median-absolute-deviation [1 1 1])))
    (is (= 1 (gp/median-absolute-deviation [1 2 3 4 5]))))

  (testing "epsilon-list is one MAD per case"
    (let [pop [{:errors [0.0 10.0]} {:errors [1.0 20.0]} {:errors [2.0 30.0]}]]
      (is (= 2 (count (gp/epsilon-list pop))))
      (is (= [1.0 10.0] (mapv double (gp/epsilon-list pop))))))

  (testing "plain lexicase DEGENERATES on continuous errors while epsilon
            lexicase does not.

            This is the whole reason epsilon lexicase exists. With float
            errors, being tied with the best on a case is a measure-zero
            event, so plain lexicase filters to one individual on the first
            case and every later case exerts no pressure at all."
    (let [;; Ten individuals, all distinct, errors spread over two cases.
          pop (mapv (fn [i] {:program (list i) :errors [(* 0.1 i) (* 0.7 i)]})
                    (range 10))
          eps (gp/epsilon-list pop)
          survivors (fn [epsilons c]
                      (let [errs (map #(nth (:errors %) c) pop)
                            best (reduce min errs)]
                        (count (filter #(<= (abs (- (nth (:errors %) c) best))
                                            (or epsilons 0.0))
                                       pop))))]
      (is (= 1 (survivors nil 0))
          "exact-equality filtering keeps exactly the one best individual")
      (is (< 1 (survivors (first eps) 0))
          "epsilon keeps a pool the remaining cases can discriminate within")))

  (testing "the six continuous-error problems ask for it by name"
    (let [epsilon-problems (set (map :name (filter #(= :epsilon-lexicase
                                                       (:selection %))
                                                   p/all-problems)))
          float-problems (set (map :name (filter #(= :push/float (:output %))
                                                 p/all-problems)))]
      (is (= float-problems epsilon-problems))
      (is (= 6 (count epsilon-problems)))))

  (testing "and a problem's choice beats the default but loses to an override"
    (is (= :epsilon-lexicase
           (get-in (gp/run-gp (p/by-name :nguyen-f1)
                              {:population-size 20 :max-generations 0
                               :n-train 5 :n-test 5})
                   [:config :selection])))
    (is (= :tournament
           (get-in (gp/run-gp (p/by-name :nguyen-f1)
                              {:population-size 20 :max-generations 0
                               :n-train 5 :n-test 5 :selection :tournament})
                   [:config :selection])))))

(deftest levenshtein-test
  (is (= 0 (p/levenshtein "abc" "abc")))
  (is (= 3 (p/levenshtein "" "abc")))
  (is (= 3 (p/levenshtein "abc" "")))
  (is (= 1 (p/levenshtein "abc" "abd")))
  (is (= 1 (p/levenshtein "abc" "ab")))
  (is (= 3 (p/levenshtein "kitten" "sitting"))))

;; ---------------------------------------------------------------------------
;; The problems are well formed
;; ---------------------------------------------------------------------------

(deftest problems-well-formed-test
  (testing "every problem generates usable cases"
    (let [reports (mapv #(run/check-reference-solution % 30 1) p/all-problems)]
      (is (every? :ok? reports)
          (str "failed: " (pr-str (remove :ok? reports))))
      (testing "with the arity its :inN instructions expect"
        (is (every? :arity-ok? reports)
            (str "arity mismatch: "
                 (pr-str (map :name (remove :arity-ok? reports))))))
      (testing "and are not degenerate - a problem whose every case has the
                same answer is solved by a constant"
        (is (every? #(<= 2 (:distinct-outputs %)) reports)
            (str "constant: "
                 (pr-str (map :name (filter #(< (:distinct-outputs %) 2)
                                            reports))))))))

  (testing "declared edge cases are actually included in the training set"
    (let [problem (p/by-name :small-or-large)
          {:keys [train]} (p/train-and-test problem 50 10 1)]
      (is (every? (set (map :inputs train))
                  (map vec (:edge-inputs problem))))))

  (testing "cases are a pure function of the seed"
    (let [problem (p/by-name :fizz-buzz)]
      (is (= (data/cases problem 20 5) (data/cases problem 20 5)))
      (is (not= (data/cases problem 20 5) (data/cases problem 20 6)))))

  (testing "the test set is drawn from different cases than the training set"
    (let [{:keys [train test]} (p/train-and-test (p/by-name :gcd) 50 50 1)]
      (is (not= (set (map :inputs train)) (set (map :inputs test)))))))

;; ---------------------------------------------------------------------------
;; Alphabets
;; ---------------------------------------------------------------------------

(deftest alphabet-test
  (testing "a stack set expands to instructions touching only those stacks"
    (let [instrs (alphabet/get-stack-instructions #{:exec :integer})]
      (is (contains? (set instrs) :integer_+))
      (is (contains? (set instrs) :exec_dup))
      (testing "and nothing that would reach a stack outside the set"
        (is (not (contains? (set instrs) :string_length)))
        (is (not (contains? (set instrs) :integer_fromfloat)))
        (is (not (contains? (set instrs) :vector_integer_length))))))

  (testing "Propeller's short stack names and ours both work"
    (is (= (alphabet/get-stack-instructions #{:integer :vector_integer})
           (alphabet/get-stack-instructions
            #{:push/integer [:push/vector :push/integer]}))))

  (testing "random instructions are excluded unless :random is asked for"
    (is (not (contains? (set (alphabet/get-stack-instructions #{:exec :integer}))
                        :integer_rand)))
    (is (contains? (set (alphabet/get-stack-instructions
                         #{:exec :integer :random}))
                   :integer_rand)))

  (testing "and so no problem's alphabet contains a stochastic instruction -
            it would make the same genome score differently each evaluation"
    (doseq [problem p/all-problems]
      (is (empty? (filter #(and (keyword? %) (re-find #"rand" (name %)))
                          (p/alphabet problem)))
          (str (:name problem) " has a random instruction"))))

  (testing "every stack an instruction touches is derived, none left blank"
    (is (empty? @alphabet/unclassified)))

  (testing "a problem's alphabet has its input instructions and its ERCs"
    (let [a (set (p/alphabet (p/by-name :grade)))]
      (is (every? a [:in1 :in2 :in3 :in4 :in5]))
      (is (not (contains? a :in6)))
      (is (every? a ["A" "B" "C" "D" "F"]))))

  (testing "atom generators are one per alphabet element, so the draw
            distribution matches Propeller's flat instruction list"
    (let [problem (p/by-name :fizz-buzz)]
      (is (= (count (p/alphabet problem))
             (count (p/atom-generators problem))))
      (is (every? fn? (p/atom-generators problem)))))

  (testing "Propeller's whole instruction inventory has an equivalent here.

            This is the completeness check for the port: PROPELLER-INSTRUCTIONS
            is Propeller's full set, assembled from the `generate-instructions`
            calls and `def-instruction` forms in
            propeller_problems/push/instructions/. Anything unresolvable is an
            instruction some problem's stack set would silently expand without."
    (let [missing (remove alphabet/propeller-name propeller-instructions)]
      (is (empty? missing) (str "no equivalent for: " (pr-str missing)))
      (is (= 362 (count propeller-instructions))
          "guard against the inventory below being edited down")))

  (testing "reading Propeller's own instruction names"
    (is (= :integer_+ (alphabet/propeller-name :integer_add)))
    (is (= :integer_- (alphabet/propeller-name :integer_subtract)))
    (is (= :integer_stackdepth (alphabet/propeller-name :integer_stack_depth)))
    (is (= :integer_yankdup (alphabet/propeller-name :integer_yank_dup)))
    (is (= :string_length (alphabet/propeller-name :string_length)))
    (is (nil? (alphabet/propeller-name :no_such_instruction)))))

;; ---------------------------------------------------------------------------
;; Running a program against a problem
;; ---------------------------------------------------------------------------

(deftest run-test
  (testing "a hand-written solution solves its problem exactly"
    (testing "smallest, answering on the PRINT stack"
      (let [problem (p/by-name :smallest)
            {:keys [train]} (p/train-and-test problem 30 5 7)]
        (is (= 30 (run/n-solved
                   problem
                   '(:in1 :in2 :integer_min :in3 :integer_min
                          :in4 :integer_min :integer_print)
                   train)))))

    (testing "small-or-large, including the dead zone"
      (let [problem (p/by-name :small-or-large)
            {:keys [train]} (p/train-and-test problem 40 5 7)]
        (is (= 40 (run/n-solved
                   problem
                   ;; :exec_if runs its SECOND exec item when true.
                   '(:in1 1000 :integer_< :exec_if
                          (:in1 2000 :integer_>= :exec_if ("") ("large"))
                          ("small"))
                   train)))))

    (testing "integer-regression"
      (let [problem (p/by-name :integer-regression)
            {:keys [train]} (p/train-and-test problem 21 5 7)]
        (is (= 21 (run/n-solved
                   problem
                   '(:in1 :in1 :integer_* :in1 :integer_*
                          :in1 :in1 :integer_* 2 :integer_* :integer_+
                          :in1 :integer_+ 3 :integer_+)
                   train))))))

  (testing "a program that outputs nothing is charged the penalty, not crashed"
    (let [problem (p/by-name :gcd)
          {:keys [train]} (p/train-and-test problem 5 5 1)
          errs (run/errors problem '() train)]
      (is (= 5 (count errs)))
      (is (every? #(= p/no-output-penalty %) errs))))

  (testing "an infinite loop is bounded by the step limit rather than hanging"
    (let [problem (p/by-name :gcd)
          {:keys [train]} (p/train-and-test problem 3 3 1)]
      (is (= 3 (count (run/errors problem '(:exec_y (:integer_dup)) train
                                  {:step-limit 200}))))))

  (testing "find-pair reads TWO integers off the stack"
    (let [problem (p/by-name :find-pair)]
      (is (zero? (:error (first (run/evaluate problem '(1 2)
                                              [{:inputs [[1 2] 3]
                                                :output [1 2]}])))))
      (testing "and only one is not an answer"
        (is (= p/no-output-penalty
               (:error (first (run/evaluate problem '(1)
                                            [{:inputs [[1 2] 3]
                                              :output [1 2]}]))))))))

  (testing "a run is reproducible from its seed.

            Both search algorithms here draw from `random-seed.core`, and both
            report their result by re-scoring a population, so a run has to be
            a pure function of its seed for any comparison between conditions
            to be attributable to the conditions."
    (let [opts {:population-size 40 :max-generations 3
                :n-train 20 :n-test 10 :seed 42}
          a (gp/run-gp (p/by-name :small-or-large) opts)
          b (gp/run-gp (p/by-name :small-or-large) opts)
          c (gp/run-gp (p/by-name :small-or-large) (assoc opts :seed 43))]
      (is (= (:best-program a) (:best-program b)))
      (is (= (:history a) (:history b)))
      (is (not= (:best-program a) (:best-program c))
          "a different seed gives a different run")))

  (testing "the multipliers score PER OUTPUT BIT, not per case"
    (let [problem (p/by-name :mul3)
          {:keys [train test]} (p/train-and-test problem 200 2000 1)]
      (testing "the case set is exhaustive over the input bits, and there is
                no test set - a circuit is correct or it is not"
        (is (= 64 (count train)))
        (is (= 0 (count test))))
      (testing "64 cases become 384 errors, one per output bit"
        (is (= 384 (count (run/errors problem '() train)))))
      (testing "an unset register is a wrong bit, not a whole-case penalty"
        (is (every? #(= 1.0 %) (run/errors problem '() train))))
      (testing "the low product bit is a0 AND b0, and scores exactly its 64"
        (is (= 64 (run/n-solved problem
                                '(:a0 :b0 :boolean_and :set_c0)
                                train))))))

  (testing "number-io's answer is printed, so it is parsed back to a number"
    (let [problem (p/by-name :number-io)
          err (p/error-fn problem)]
      (is (zero? (err 3.5 "3.5")))
      (is (= 0.5 (err 3.5 "3.0")))
      (is (= p/no-output-penalty (err 3.5 "not a number"))))))
