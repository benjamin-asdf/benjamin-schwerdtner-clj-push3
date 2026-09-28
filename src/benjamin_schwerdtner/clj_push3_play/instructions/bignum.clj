(ns benjamin-schwerdtner.clj-push3-play.instructions.bignum
  "An unbounded integer stack - the output channel for busy-beaver organisms.

  Chaitin's metabiology needs a fitness that can keep going up forever:
  the whole claim is that organism fitness grows faster than any computable
  function. `:push/integer` is clamped to +-1e10, which flattens that landscape
  after a few hundred execution steps. BIGNUM carries java.math.BigInteger and
  is bounded only by the `:max-bignum-bits` bit budget (see
  benjamin-schwerdtner.clj-push3-play.stack.pushvalues).

  Naming big numbers, in Chaitin's ladder: N, N + N, N * N, N^N, N^N^N ...
  The primitives here are deliberately just the rungs of that ladder plus stack
  plumbing; the towers themselves are for organisms to discover (with :exec_y
  for recursion).

  There are no BIGNUM literals on purpose. Organisms lift values in from the
  INTEGER stack with :bignum_frominteger, which keeps programs plain edn - a
  BigInteger literal would read back as a Long and silently change type when an
  organism is round-tripped through a results file."
  (:require [benjamin-schwerdtner.clj-push3-play.instructions.impl
             :refer [register-instruction] :as impl]
            [benjamin-schwerdtner.clj-push3-play.stack.pushvalues :as pushvalues]))

;; Arg convention follows instructions/numbers.clj: `a` is the top of the
;; stack, `b` the second item.

(register-instruction
 {:sym-name :bignum_frominteger
  :in [:push/integer]
  :out :push/bignum
  :doc "Lift the top INTEGER onto the BIGNUM stack."
  :f (fn [_ a] (biginteger a))})

(register-instruction
 {:sym-name :bignum_+
  :in [:push/bignum :push/bignum]
  :out :push/bignum
  :doc "Pushes the sum of the top two BIGNUMs."
  :f (fn [_ a b] (.add a b))})

(register-instruction
 {:sym-name :bignum_*
  :in [:push/bignum :push/bignum]
  :out :push/bignum
  :doc "Pushes the product of the top two BIGNUMs."
  :f (fn [_ a b] (.multiply a b))})

(register-instruction
 {:sym-name :bignum_-
  :in [:push/bignum :push/bignum]
  :out :push/bignum
  :doc "Pushes the difference of the top two BIGNUMs."
  :f (fn [_ a b] (.subtract a b))})

(register-instruction
 {:sym-name :bignum_succ
  :in [:push/bignum]
  :out :push/bignum
  :doc "Pushes the top BIGNUM plus one."
  :f (fn [_ a] (.add a java.math.BigInteger/ONE))})

;; ---------------------------------------------------------------------------
;; Exponentiation needs a guard *before* the multiply, not after: (.pow a n)
;; with a large n allocates the full result before any sanitizer could clamp
;; it. bitLength(a^n) = n * bitLength(a) (within one bit), which is cheap to
;; check against the budget.
;; ---------------------------------------------------------------------------

(def ^:private max-exponent
  "Cap on the exponent itself, so the width projection below cannot overflow a
  long even at an absurd bit budget."
  (long 1e6))

(defn pow-guarded
  "a^n, saturated to the state's bit budget rather than allocated in full.

  Negative exponents are treated as their absolute value: the BIGNUM stack is
  integral, and rounding 1/a^n to zero would just throw the organism's work
  away."
  [state a n]
  (let [n (min max-exponent (abs (long n)))
        budget (pushvalues/bignum-bit-budget state)
        ;; bitLength of the magnitude: 0 for 0, 1 for +-1, 2 for +-2 ...
        width (.bitLength (.abs a))]
    (cond
      (zero? n) java.math.BigInteger/ONE
      ;; 0, 1 and -1 never grow - and their width projection would lie.
      (<= width 1) (.pow a (int n))
      (< budget (* n width))
      (pushvalues/saturate-bignum budget
                                  (if (and (neg? (.signum a)) (odd? n)) -1 1))
      :else (.pow a (int n)))))

(register-instruction
 {:sym-name :bignum_pow
  :in [:push/bignum :push/integer]
  :out :push/bignum
  :doc "Pushes the top BIGNUM raised to the top INTEGER."
  :f (fn [state a n] (pow-guarded state a n))})

;; ---------------------------------------------------------------------------

(register-instruction
 {:sym-name :bignum_bitlength
  :in [:push/bignum]
  :out :push/integer
  :doc "Pushes the bit length of the top BIGNUM onto INTEGER. Lets an organism
  measure how big a number it has named so far."
  :f (fn [_ a] (.bitLength (.abs a)))})

(register-instruction
 {:sym-name :bignum_max
  :in [:push/bignum :push/bignum]
  :out :push/bignum
  :doc "Pushes the larger of the top two BIGNUMs."
  :f (fn [_ a b] (.max a b))})

(register-instruction
 {:sym-name :bignum_<
  :in [:push/bignum :push/bignum]
  :out :push/boolean
  :doc "Pushes true if the second BIGNUM is less than the top BIGNUM."
  :f (fn [_ a b] (neg? (.compareTo b a)))})

;; Stack plumbing
(doseq [[sym-name impl]
        [[:bignum_stackdepth impl/stack-depth-impl]
         [:bignum_pop impl/popper]
         [:bignum_dup impl/dupper]
         [:bignum_flush impl/flusher]
         [:bignum_shove impl/shover]
         [:bignum_yank impl/yanker]
         [:bignum_yankdup impl/yankdupper]
         [:bignum_rot impl/rotater]
         [:bignum_swap impl/swapper]
         [:bignum_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/bignum})))

(comment
  ;; The Chaitin ladder, by hand. Repeated squaring of 2:
  (require '[benjamin-schwerdtner.clj-push3-play.interpreter :as push])

  (let [s (push/execute (push/setup-state)
                        '(2 :bignum_frominteger
                            :exec_y (:bignum_dup :bignum_*))
                        {:max-executions 250})]
    (.bitLength (peek (-> s :stacks :push/bignum))))
  ;; 100000 - saturated at the bit budget, where INTEGER would have stopped
  ;; at 1e10 (34 bits).
  )
