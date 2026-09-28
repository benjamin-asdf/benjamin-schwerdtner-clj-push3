(ns benjamin-schwerdtner.clj-push3-play.instructions.boolean
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]))

;; Boolean logic instructions

(register-instruction
 {:sym-name :boolean_and
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (and a b))})

(register-instruction
 {:sym-name :boolean_or
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (or a b))})

(register-instruction
 {:sym-name :boolean_not
  :in [:push/boolean]
  :out :push/boolean
  :f (fn [_ a] (not a))})

(register-instruction
 {:sym-name :boolean_=
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

(register-instruction
 {:sym-name :boolean_xor
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (not= a b))})

;; AND with one argument negated. These look like clutter next to
;; :boolean_not, and they are not: NAND and its relatives are the gates a
;; boolean circuit is actually built from, and reaching one as
;; `not-then-and` costs two instructions that have to be adjacent. In the
;; multiplier benchmarks that adjacency is most of the difficulty.
;;
;; `a` is the top of the stack and `b` the second item, so `first` and
;; `second` here name the stack positions, not the argument positions.
(register-instruction
 {:sym-name :boolean_invert_first_then_and
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (and (not a) b))})

(register-instruction
 {:sym-name :boolean_invert_second_then_and
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (and a (not b)))})

;; The negated gates. Kalkreuth et al. recommend BUFa, NOTa, AND, OR, XOR,
;; NAND, NOR, XNOR as the alphabet for boolean circuit synthesis, and the
;; multiplier benchmarks use exactly that set. NAND and NOR are each
;; universal, so having them directly is the difference between a one-token
;; gate and a two-token composition that mutation has to keep adjacent.

(register-instruction
 {:sym-name :boolean_nand
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (not (and a b)))})

(register-instruction
 {:sym-name :boolean_nor
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (not (or a b)))})

(register-instruction
 {:sym-name :boolean_xnor
  :in [:push/boolean :push/boolean]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

;; Type conversion instructions

(register-instruction
 {:sym-name :boolean_fromfloat
  :in [:push/float]
  :out :push/boolean
  :f (fn [_ x] (not (zero? x)))})

(register-instruction
 {:sym-name :boolean_frominteger
  :in [:push/integer]
  :out :push/boolean
  :f (fn [_ x] (not (zero? x)))})

(register-instruction
 {:sym-name :boolean_rand
  :in []
  :out :push/boolean
  :f (fn [_] (gen/rand-bool))})

;; ---------------------------------

(doseq [[sym-name impl]
        [[:boolean_stackdepth impl/stack-depth-impl]
         [:boolean_pop impl/popper]
         [:boolean_dup impl/dupper]
         [:boolean_flush impl/flusher]
         [:boolean_shove impl/shover]
         [:boolean_yank impl/yanker]
         [:boolean_yankdup impl/yankdupper]
         [:boolean_rot impl/rotater]
         [:boolean_swap impl/swapper]
         [:boolean_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/boolean})))
