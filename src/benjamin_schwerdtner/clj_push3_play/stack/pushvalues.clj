(ns benjamin-schwerdtner.clj-push3-play.stack.pushvalues)

(defmulti sanitize-value (fn [_ push-type v] push-type))

(defmethod sanitize-value :default [_ _ v] v)

(defmethod sanitize-value :push/integer [_ _ v]
  (max (long -1e10) (min (long 1e10) v)))

(defmethod sanitize-value :push/float [_ _ v]
  (let [v (max -1e12 (min 1e12 v))]
    (if (< (abs v) 1e-12)
      (float 1e-12)
      v)))

(defmethod sanitize-value :push/code [state _ v]
  ;; check max code points
  v)

;; -------------------------------------------------------------------
;; BIGNUM
;;
;; `:push/integer` is clamped to +-1e10 above. That clamp is fatal for the
;; busy-beaver landscape: an organism reaches the ceiling in a few hundred
;; execution steps and from there on every fitter organism looks equally fit.
;;
;; BIGNUM carries java.math.BigInteger instead, bounded only by a bit budget.
;; Values wider than the budget saturate instead of exhausting the heap.
;; -------------------------------------------------------------------

(def default-max-bignum-bits
  "Default bit budget for the BIGNUM stack. ~30k decimal digits."
  (long 1e5))

(defn bignum-bit-budget
  [state]
  (get-in state [:parameters :max-bignum-bits] default-max-bignum-bits))

(def ^:private saturation-value
  (memoize (fn [bits]
             (.subtract (.shiftLeft java.math.BigInteger/ONE bits)
                        java.math.BigInteger/ONE))))

(defn saturate-bignum
  "Returns the widest BIGNUM permitted by `bits`, with the sign of `signum`."
  [bits signum]
  (let [v (saturation-value bits)]
    (if (neg? signum) (.negate v) v)))

(defmethod sanitize-value :push/bignum [state _ v]
  (let [v (if (instance? java.math.BigInteger v) v (biginteger v))
        budget (bignum-bit-budget state)]
    (if (< budget (.bitLength v))
      (saturate-bignum budget (.signum v))
      v)))
