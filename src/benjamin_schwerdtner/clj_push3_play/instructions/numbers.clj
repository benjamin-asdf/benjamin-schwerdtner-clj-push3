(ns benjamin-schwerdtner.clj-push3-play.instructions.numbers
  ;; `rand`/`rand-int` come from random-seed.core so that :integer_rand and
  ;; :float_rand draw from the interpreter's seeded `*rng*` like every other
  ;; stochastic instruction. With clojure.core's versions they were unseeded,
  ;; which silently made any run containing them irreproducible.
  (:refer-clojure :exclude [rand rand-int])
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [random-seed.core :refer [rand rand-int]]))

;; Arithmetic instructions

;;
;; ---
;; Note
;;
;; - 1. we use the auto promoting functions, allowing bigint
;; - 2. long, bigint, integer count as push 'integer' type
;; - 3. float, double, ratio count as push 'float' type

;; -----------------

;; Mod / Div:
;;
;; Official spec is a bit complicated.
;; I take the freedom to implement a simpler, more regular version.
;;
;; - 1. pop 2 numbers from the stack
;; - 2. if the second item is zero, leave state (_with items popped_)
;; - 3. else push the result of modding the first by the second value
;;

;; In the orig spec, a zero value is a NOOP.
;; In this version, a zero value still pops the items.

;; This concerns modder and divider.

;; NOTE on argument order, because it was wrong here and in [[divider]] until
;; 2026-09-02, and it is easy to get wrong again.
;;
;; `execute-instruction` pops the `:in` stacks left to right, so for
;; `[push-type push-type]` the FIRST argument is the TOP of the stack and the
;; second argument is the item below it. The spec - and the docstrings here -
;; describe these operators as "the SECOND stack item <op> the TOP stack
;; item", so the operands appear in the arglist in the reverse of the order
;; they are written in. The comparison generators below already did this
;; correctly, naming their arguments `[_state b a]` to make it visible; these
;; two did not, and computed `top mod second` and `top / second`.
;;
;; The divisor is the TOP item, so it is the top item that is checked for zero.

(defn modder [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item modulo the top stack item"
   :f (fn [state divisor dividend]
        (if (zero? divisor)
          state
          (stack/push state push-type (mod dividend divisor))))
   :in [push-type push-type]
   :out :state
   :sym-name sym-name})

(defn multiplier [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item multiplied by the top stack item"
   :f (fn [_state a b]
        (*' a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

(defn adder [{:keys [sym-name push-type]}]
  {:doc "Pushes the sum of the top two stack items"
   :f (fn [_state a b]
        (+' a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

(defn subtractor [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item minus the top stack item"
   ;; Same reversal as [[modder]] above: `a` is the top of the stack, so the
   ;; spec's "second item minus the top item" is `(- b a)`. This used to be
   ;; `(-' a b)` and computed the negation of what it advertised. `+`, `*`,
   ;; `min` and `max` are commutative and were unaffected.
   :f (fn [_state a b]
        (-' b a))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

#_(defn divider
    [{:keys [sym-name push-type]}]
    {:doc "Pushes the second stack item divided by the top stack item"
     :f (fn [state]
          ;; f the top item is zero this acts as a NOOP
          (if (or (empty? (-> state :stacks push-type))
                  (zero? (stack/peek-item state push-type)))
            state
            (let [a (stack/stack-nth state push-type 0)
                  b (stack/stack-nth state push-type 1)]
              (-> state
                  (stack/pop-n push-type 2)
                  (stack/push push-type (/ b a))))))
     :in []
     :out :state
     :sym-name sym-name})

(defn divider
  [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item divided by the top stack item"
   :f (fn [state divisor dividend]
        ;; If the top item is zero this acts as a NOOP
        ;; (still pops items in this version)
        (if (zero? divisor)
          state
          (stack/push state
                      push-type
                      (if (= push-type :push/integer)
                        (bigint (/ (Math/floor dividend) (Math/floor divisor)))
                        (/ dividend divisor)))))
   :in [push-type push-type]
   :out :state
   :sym-name sym-name})

;; -----------
;; Comparison instructions

(defn lt-er [{:keys [sym-name push-type]}]
  {:doc "Pushes true if the second stack item is less than the top stack item"
   :f (fn [_state b a]
        (< a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

(defn eq-er
  [{:keys [sym-name push-type]}]
  {:doc
   "Pushes true if the second stack item is equal to the top stack item"
   :f (fn [_state a b] (= a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

(defn gt-er
  [{:keys [sym-name push-type]}]
  {:doc
   "Pushes true if the second stack item is greater than the top stack item"
   :f (fn [_state b a] (> a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

;; Min/Max instructions

(defn minner [{:keys [sym-name push-type]}]
  {:doc "Pushes the minimum of the top two stack items"
   :f (fn [_state a b] (min a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

(defn maxxer
  [{:keys [sym-name push-type]}]
  {:doc "Pushes the maximum of the top two stack items"
   :f (fn [_state a b] (max a b))
   :in [push-type push-type]
   :out push-type
   :sym-name sym-name})

;; -----------
;; The remaining PushGP arithmetic. `<`, `>` and `=` were here; the ones
;; below were not, and their absence is not neutral. Counting loops are
;; written against `>=`/`<=` far more often than against a strict
;; comparison plus an off-by-one correction, and `inc`/`dec` are how a
;; program walks an index without having to first find the literal 1.

(defn gte-er [{:keys [sym-name push-type]}]
  {:doc "Pushes true if the second stack item is >= the top stack item"
   :f (fn [_state b a] (>= a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

(defn lte-er [{:keys [sym-name push-type]}]
  {:doc "Pushes true if the second stack item is <= the top stack item"
   :f (fn [_state b a] (<= a b))
   :in [push-type push-type]
   :out :push/boolean
   :sym-name sym-name})

(defn incer [{:keys [sym-name push-type]}]
  {:doc "Pushes the top stack item plus one"
   :f (fn [_state a] (inc' a))
   :in [push-type]
   :out push-type
   :sym-name sym-name})

(defn decer [{:keys [sym-name push-type]}]
  {:doc "Pushes the top stack item minus one"
   :f (fn [_state a] (dec' a))
   :in [push-type]
   :out push-type
   :sym-name sym-name})

(defn quoter
  [{:keys [sym-name push-type]}]
  {:doc "Pushes the second stack item divided by the top, truncated toward zero"
   ;; Distinct from [[divider]], which floors both operands before dividing
   ;; and so disagrees with this on negative inputs. `quot` is what the PSB
   ;; reference solutions use, so it is the one the expected outputs were
   ;; generated with.
   :f (fn [state divisor dividend]
        (if (zero? divisor)
          state
          (stack/push state push-type (quot dividend divisor))))
   :in [push-type push-type]
   :out :state
   :sym-name sym-name})

;; RENAMED:
;; integer_% -> integer_mod
;; integer_/ -> integer_div

(doseq [[sym-name impl]
        [[:integer_mod modder]
         [:integer_* multiplier]
         [:integer_+ adder]
         [:integer_- subtractor]
         [:integer_div divider]
         ;; --
         [:integer_quot quoter]
         [:integer_inc incer]
         [:integer_dec decer]
         ;; --
         [:integer_< lt-er]
         [:integer_> gt-er]
         [:integer_= eq-er]
         [:integer_>= gte-er]
         [:integer_<= lte-er]
         ;; --
         [:integer_min minner]
         [:integer_max maxxer]]]

  (register-instruction
   (impl {:push-type :push/integer
          :sym-name sym-name})))

;; -----------------------------------

;; Type conversion instructions

(register-instruction
 {:sym-name :integer_fromboolean
  :in [:push/boolean]
  :out :push/integer
  :f (fn [_ bool] (if bool 1 0))})

(register-instruction
 {:sym-name :integer_fromfloat
  :in [:push/float]
  :out :push/integer
  :f (fn [_ x] (long x))})

(register-instruction
 {:sym-name :integer_fromchar
  :in [:push/char]
  :out :push/integer
  :f (fn [_ c] (long (int c)))})

;; Parsing a string to a number is how several of the PSB problems read their
;; input, and it is the one conversion that can fail. A failed parse leaves the
;; state alone APART from having consumed the string - the same contract as
;; :integer_div by zero above. Returning the state unchanged including the
;; string would make the instruction a no-op that a program could sit on
;; forever inside an :exec_y loop.
(defn- parse-number
  [s]
  (try
    (let [v (read-string s)]
      (when (number? v) v))
    (catch Exception _ nil)))

(register-instruction
 {:sym-name :integer_fromstring
  :in [:push/string]
  :out :state
  :f (fn [state s]
       (if-let [v (parse-number s)]
         (stack/push state :push/integer (long v))
         state))})

;; Random instruction

(register-instruction
 {:sym-name :integer_rand
  :in []
  :out :push/integer
  :f (fn [state]
       (let [min-val (get-in state [:parameters :min-random-integer] Integer/MIN_VALUE)
             max-val (get-in state [:parameters :max-random-integer] Integer/MAX_VALUE)]
         (+ min-val (rand-int (inc (- max-val min-val))))))})

;; -----------------------------------

;; Stack manipulation instructions using impl generators

(doseq [[sym-name impl]
        [[:integer_stackdepth impl/stack-depth-impl]
         [:integer_pop impl/popper]
         [:integer_dup impl/dupper]
         [:integer_flush impl/flusher]
         [:integer_shove impl/shover]
         [:integer_yank impl/yanker]
         [:integer_yankdup impl/yankdupper]
         [:integer_rot impl/rotater]
         [:integer_swap impl/swapper]
         [:integer_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/integer})))

;; ==================================

;; Float instructions

(doseq [[sym-name impl]
        [[:float_mod modder]
         [:float_* multiplier]
         [:float_+ adder]
         [:float_- subtractor]
         [:float_div divider]
         ;; --
         [:float_quot quoter]
         [:float_inc incer]
         [:float_dec decer]
         ;; --
         [:float_< lt-er]
         [:float_> gt-er]
         [:float_= eq-er]
         [:float_>= gte-er]
         [:float_<= lte-er]
         ;; --
         [:float_min minner]
         [:float_max maxxer]]]
  (register-instruction
   (impl {:push-type :push/float
          :sym-name sym-name})))

;; -----------------------------------

;; Float type conversion instructions

(register-instruction
 {:sym-name :float_fromboolean
  :in [:push/boolean]
  :out :push/float
  :f (fn [_ bool] (if bool 1.0 0.0))})

(register-instruction
 {:sym-name :float_frominteger
  :in [:push/integer]
  :out :push/float
  :f (fn [_ x] (double x))})

(register-instruction
 {:sym-name :float_fromchar
  :in [:push/char]
  :out :push/float
  :f (fn [_ c] (double (int c)))})

(register-instruction
 {:sym-name :float_fromstring
  :in [:push/string]
  :out :state
  :f (fn [state s]
       (if-let [v (parse-number s)]
         (stack/push state :push/float (double v))
         state))})

;; Float random instruction

(register-instruction
 {:sym-name :float_rand
  :in []
  :out :push/float
  :f (fn [state]
       (let [min-val (get-in state [:parameters :min-random-float] -1.0)
             max-val (get-in state [:parameters :max-random-float] 1.0)]
         (+ min-val (* (rand) (- max-val min-val)))))})

;; -----------------------------------

;; Float trigonometric instructions

(register-instruction
 {:sym-name :float_cos
  :in [:push/float]
  :out :push/float
  :f (fn [_ x] (Math/cos x))})

(register-instruction
 {:sym-name :float_sin
  :in [:push/float]
  :out :push/float
  :f (fn [_ x] (Math/sin x))})

(register-instruction
 {:sym-name :float_tan
  :in [:push/float]
  :out :push/float
  :f (fn [_ x] (Math/tan x))})

;; -----------------------------------

;; Float stack manipulation instructions using impl generators

(doseq [[sym-name impl]
        [[:float_stackdepth impl/stack-depth-impl]
         [:float_pop impl/popper]
         [:float_dup impl/dupper]
         [:float_flush impl/flusher]
         [:float_shove impl/shover]
         [:float_yank impl/yanker]
         [:float_yankdup impl/yankdupper]
         [:float_rot impl/rotater]
         [:float_swap impl/swapper]
         [:float_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/float})))
