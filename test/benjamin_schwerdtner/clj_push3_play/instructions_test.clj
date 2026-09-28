(ns benjamin-schwerdtner.clj-push3-play.instructions-test
  "Tests for the instructions added so that the PSB1/PSB2 benchmarks can be
  expressed, and for the arithmetic operand order they depend on."
  (:require
   [clojure.test :refer [deftest testing is]]
   [benjamin-schwerdtner.clj-push3-play.interpreter :as push]
   [benjamin-schwerdtner.clj-push3-play.instructions.input-output :as io]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as instructions]))

(defn run
  "Execute `program` with `inputs` bound for :in1.., return the final state."
  ([program] (run program []))
  ([program inputs]
   (-> (push/setup-state)
       (io/with-inputs inputs)
       (push/execute program {:max-executions 2000}))))

(defn stack-of [state t] (get-in state [:stacks t]))

;; ---------------------------------------------------------------------------
;; Arithmetic operand order
;;
;; Push3 specifies these as "the SECOND stack item <op> the TOP stack item".
;; `integer_-`, `integer_mod` and `integer_div` all computed the reverse until
;; 2026-09-02, which is invisible for the commutative operators and wrong for
;; every one of these. Pinned here because the benchmarks' expected outputs
;; were generated against the spec order.
;; ---------------------------------------------------------------------------

(deftest arithmetic-operand-order-test
  (testing "non-commutative integer arithmetic follows second-op-top"
    (is (= [5] (stack-of (run '(7 2 :integer_-)) :push/integer)))
    (is (= [1] (stack-of (run '(7 2 :integer_mod)) :push/integer)))
    (is (= [3] (stack-of (run '(7 2 :integer_quot)) :push/integer)))
    (is (= [3N] (stack-of (run '(7 2 :integer_div)) :push/integer))))
  (testing "and on floats"
    (is (= [5.0] (stack-of (run '(7.0 2.0 :float_-)) :push/float)))
    (is (= [0.25] (stack-of (run '(1.0 4.0 :float_div)) :push/float))))
  (testing "commutative ones are unaffected"
    (is (= [9] (stack-of (run '(7 2 :integer_+)) :push/integer)))
    (is (= [14] (stack-of (run '(7 2 :integer_*)) :push/integer))))
  (testing "a zero divisor consumes its arguments and pushes nothing"
    (is (= [] (stack-of (run '(7 0 :integer_div)) :push/integer)))
    (is (= [] (stack-of (run '(7 0 :integer_mod)) :push/integer))))
  (testing "comparisons, which were already right"
    (is (= [true] (stack-of (run '(3 5 :integer_<)) :push/boolean)))
    (is (= [true] (stack-of (run '(5 3 :integer_>)) :push/boolean)))
    (is (= [true] (stack-of (run '(3 3 :integer_>=)) :push/boolean)))
    (is (= [false] (stack-of (run '(5 3 :integer_<=)) :push/boolean))))
  (testing "inc and dec"
    (is (= [4] (stack-of (run '(3 :integer_inc)) :push/integer)))
    (is (= [2] (stack-of (run '(3 :integer_dec)) :push/integer)))))

;; ---------------------------------------------------------------------------
;; INPUT
;; ---------------------------------------------------------------------------

(deftest input-instructions-test
  (testing "an input can be read more than once"
    (is (= [49] (stack-of (run '(:in1 :in1 :integer_*) [7]) :push/integer))))
  (testing "one instruction serves every type, because it goes via EXEC"
    (is (= [5] (stack-of (run '(:in1 :string_length) ["hello"]) :push/integer)))
    (is (= [4] (stack-of (run '(:in1 :vector_integer_length) [[1 2 3 4]])
                         :push/integer)))
    (is (= [true] (stack-of (run '(:in1) [true]) :push/boolean))))
  (testing "several inputs, numbered from one"
    (is (= [30] (stack-of (run '(:in1 :in2 :in3 :integer_+ :integer_+)
                               [10 20 0])
                          :push/integer))))
  (testing "inputs may be given as a map"
    (is (= [7] (stack-of (run '(:in2) {:in2 7}) :push/integer))))
  (testing "an input with no value is a NOOP, not an exception"
    (is (= [] (stack-of (run '(:in4) [1]) :push/integer)))))

;; ---------------------------------------------------------------------------
;; PRINT
;; ---------------------------------------------------------------------------

(deftest print-instructions-test
  (testing "printing nothing is the empty string, not a missing item"
    (is (= "" (io/printed (run '())))))
  (testing "numbers print through pr-str, strings and chars as themselves"
    (is (= "42" (io/printed (run '(42 :integer_print)))))
    (is (= "ab" (io/printed (run '("ab" :string_print)))))
    (is (= "x" (io/printed (run '(\x :char_print)))))
    (is (= "true" (io/printed (run '(true :boolean_print)))))
    (is (= "[1 2]" (io/printed (run '([1 2] :vector_integer_print))))))
  (testing "output accumulates in order"
    (is (= "ab\ncd"
           (io/printed (run '("ab" :string_print :print_newline
                                   "cd" :string_print))))))
  (testing "printing pops the value it printed"
    (is (= [] (stack-of (run '(42 :integer_print)) :push/integer)))))

;; ---------------------------------------------------------------------------
;; Named inputs and output registers
;; ---------------------------------------------------------------------------

(deftest named-input-test
  (testing "any keyword in the input map resolves to its value"
    (is (= [true] (stack-of (run '(:a2) {:a2 true}) :push/boolean)))
    (is (= [7] (stack-of (run '(:foo) {:foo 7}) :push/integer))))
  (testing "a named input can be read repeatedly, like :inN"
    (is (= [true true] (stack-of (run '(:a0 :a0) {:a0 true}) :push/boolean))))
  (testing "a keyword NOT in the input map is still an ordinary literal"
    (is (= [:a2] (stack-of (run '(:a2) {}) :push/clj-object))))
  (testing "and an input name can never shadow a real instruction"
    ;; :integer_+ is in the table, so it stays an instruction even when the
    ;; input map has an entry of that name.
    (is (= [3] (stack-of (run '(1 2 :integer_+) {:integer_+ 99})
                         :push/integer)))))

(deftest output-register-test
  (testing "set_cN writes a register, cN reads it back"
    (let [state (run '(true :set_c0 false :set_c5))]
      (is (= {:c0 true :c5 false} (:push/output state)))
      (is (= [] (stack-of state :push/boolean)) "set_cN pops its argument")))
  (testing "reading gives the value back onto BOOLEAN"
    (is (= [true] (stack-of (run '(true :set_c3 :c3)) :push/boolean))))
  (testing "an UNSET register pushes nothing - it is not false"
    (is (= [] (stack-of (run '(:c7)) :push/boolean))))
  (testing "registers read out MSB first, with :unset for what was not written"
    (is (= [false :unset :unset :unset :unset true]
           (io/output-registers (run '(true :set_c0 false :set_c5)) 6))))
  (testing "set_cN with an empty BOOLEAN stack is a NOOP"
    (is (= {} (:push/output (run '(:set_c0)))))))

;; ---------------------------------------------------------------------------
;; The polymorphic operators
;; ---------------------------------------------------------------------------

(deftest polymorphic-instructions-test
  (testing "_empty asks whether a stack is empty"
    (is (= [true false]
           (stack-of (run '(:integer_empty 5 :integer_empty)) :push/boolean)))
    (is (= [true] (stack-of (run '(:string_empty)) :push/boolean))))
  (testing "_dup_times leaves n copies"
    (is (= [9 9 9] (stack-of (run '(9 3 :integer_dup_times)) :push/integer)))
    (is (= [9] (stack-of (run '(9 1 :integer_dup_times)) :push/integer)))
    (testing "and n<=0 pops instead"
      (is (= [] (stack-of (run '(9 0 :integer_dup_times)) :push/integer)))
      (is (= [] (stack-of (run '(9 -3 :integer_dup_times)) :push/integer))))
    (testing "a huge n cannot blow the heap"
      (is (>= 1001 (count (stack-of (run '(9 100000000 :integer_dup_times))
                                    :push/integer))))))
  (testing "_dup_items duplicates the top n, in order"
    (is (= [1 2 3 2 3]
           (stack-of (run '(1 2 3 2 :integer_dup_items)) :push/integer)))
    (testing "n beyond the stack duplicates all of it"
      (is (= [1 2 1 2]
             (stack-of (run '(1 2 9 :integer_dup_items)) :push/integer)))))
  (testing "_deep_dup indexes from the bottom"
    (is (= [10 20 30 10]
           (stack-of (run '(10 20 30 0 :integer_deep_dup)) :push/integer)))
    (is (= [10 20 30 20]
           (stack-of (run '(10 20 30 1 :integer_deep_dup)) :push/integer)))
    (testing "and clamps an out-of-range index"
      (is (= [10 20 30 30]
             (stack-of (run '(10 20 30 99 :integer_deep_dup))
                       :push/integer)))))
  (testing "these exist on the vector stacks too"
    (is (= [true] (stack-of (run '(:vector_integer_empty)) :push/boolean)))))

;; ---------------------------------------------------------------------------
;; Boolean gates
;; ---------------------------------------------------------------------------

(deftest boolean-instructions-test
  (is (= [true] (stack-of (run '(true false :boolean_xor)) :push/boolean)))
  (is (= [false] (stack-of (run '(true true :boolean_xor)) :push/boolean)))
  (testing "the negated gates, which is what a circuit is built from"
    (is (= [true] (stack-of (run '(true false :boolean_nand)) :push/boolean)))
    (is (= [false] (stack-of (run '(true true :boolean_nand)) :push/boolean)))
    (is (= [false] (stack-of (run '(true false :boolean_nor)) :push/boolean)))
    (is (= [true] (stack-of (run '(false false :boolean_nor)) :push/boolean)))
    (is (= [true] (stack-of (run '(true true :boolean_xnor)) :push/boolean)))
    (is (= [false] (stack-of (run '(true false :boolean_xnor)) :push/boolean))))
  (testing "invert-then-and, where `first` names the top of the stack"
    (is (= [true] (stack-of (run '(true false :boolean_invert_first_then_and))
                            :push/boolean)))
    (is (= [true] (stack-of (run '(false true :boolean_invert_second_then_and))
                            :push/boolean)))))

;; ---------------------------------------------------------------------------
;; Type conversions the benchmarks read their inputs with
;; ---------------------------------------------------------------------------

(deftest conversion-instructions-test
  (is (= [97] (stack-of (run '(\a :integer_fromchar)) :push/integer)))
  (is (= [42] (stack-of (run '("42" :integer_fromstring)) :push/integer)))
  (is (= [1.5] (stack-of (run '("1.5" :float_fromstring)) :push/float)))
  (testing "an unparseable string is consumed and pushes nothing"
    (is (= [] (stack-of (run '("abc" :integer_fromstring)) :push/integer))))
  (is (= [""] (stack-of (run '(:string_empty_string)) :push/string))))

;; ---------------------------------------------------------------------------
;; NAME resolution
;;
;; `handle-name` read `(get state :bindings identifier)`, i.e. the bindings map
;; with the identifier as a DEFAULT, so it was always truthy and no name ever
;; reached the NAME stack - which made every *_define instruction unreachable.
;; ---------------------------------------------------------------------------

(deftest name-resolution-test
  (testing "an unbound name lands on the NAME stack"
    (is (= '[foo] (stack-of (run '(foo)) :push/name))))
  (testing "a bound name is looked up, so define works"
    (is (= [10] (stack-of (run '(5 x :integer_define x x :integer_+))
                          :push/integer))))
  (testing "the bindings map itself never reaches a stack"
    (is (empty? (stack-of (run '(foo)) :push/clj-object)))))

;; ---------------------------------------------------------------------------
;; Registration
;; ---------------------------------------------------------------------------

(deftest vector-stacks-registered-test
  (testing "instructions/vector.clj is reachable through the interface, which
            it was not before - the namespace existed but nothing required it"
    (let [all (set (instructions/all-instructions))]
      (is (contains? all :vector_integer_length))
      (is (contains? all :vector_string_conj))
      (is (contains? all :vector_float_nth))))
  (testing "and they work"
    (is (= [6] (stack-of (run '([1 2 3] :vector_integer_pushall
                                        :integer_+ :integer_+))
                         :push/integer)))))
