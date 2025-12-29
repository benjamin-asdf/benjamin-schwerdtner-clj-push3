(ns benjamin-schwerdtner.clj-push3-play.hdc.fractional-power-encoding
  (:require [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
            [benjamin-schwerdtner.clj-push3-play.hdc.config :refer
             [*torch-device*]]
            [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :refer [py. py..] :as py]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

;; for cleanup:
;; https://arxiv.org/html/2412.00488v1

(defn fractional-power-encoding
  "Computes the fractional power of a hypervector x raised to power k.
   For FHRR, this is done by multiplying the phase angles by k.

   This implements x^k where k can be any real number or tensor of real numbers.

   Args:
     x: FHRR hypervector (complex tensor) with shape [..., dimensions]
     k: real number exponent or tensor of exponents
        - If scalar: applies same power to all elements of x
        - If tensor: must be broadcastable with x
        - Common use cases:
          * Scalar k: same power for entire vector
          * Tensor k with shape [1]: same as scalar
          * Tensor k with compatible shape for broadcasting

   Returns:
     FHRR hypervector representing x^k with same shape as x

   Examples:
     ;; Scalar exponent - same power for all elements
     (fractional-power-encoding x 2.5)

     ;; Tensor with single value - equivalent to scalar
     (fractional-power-encoding x (torch/tensor [2.5]))


(let [a (hd/seed)
      b (hd/seed)]
     [
      ;; pretty cool effect: A smooth similarity :

      ;; 0          |  0-1                    | 1          | 1-2       | 2
      ;; dissimilar | smoothly more similiar  | identical  | dito less | dissimilar
      (into []
            (py..
                (torch/cat
                 (vec
                  (for [k (range 0 2.2 0.1)]
                    (hd/dot-similarity a (fractional-power-encoding a k)))))
                flatten
                tolist))])

[[-66.85767364501953 1037.0406494140625 2293.38232421875
  3643.3388671875 5018.9560546875 6347.482421875 7556.1494140625
  8577.060546875 9351.818359375 9835.544921875

  10000.0

  9835.544921875
  9351.8173828125 8577.060546875 7556.1494140625 6347.482421875
  5018.95556640625 3643.337890625 2293.382568359375 1037.0408935546875
  -66.8578109741211 -972.173095703125]]

  ------------------------------


  Also it holds:

  1)  ∀(a,b)∈H: (a ⊗ b) ⊗ fractional-power-encoding(a, -1) = (a ⊗ b) ⊘ a = b

  where
  ⊗ is [[bind]]
  ⊘ is [[unbind]].

  2)  ∀(a)∈H:  fractional-power-encoding(a, -1) = inverse(a).

  Strictly speaking this is approximate, but when the vectors are unitary, like by [[hd/seed]],
  it is the true inverse.


  Also, like with exponents in R:


  3) x^0  = `ones` ; 1

  4) x^1  = x

  5) x^-1 = inverse(x) ; 1/x

  6) x^k1 * x^k2 = x^(k1 + k2)

  where

    ^ is fractional-power-encoding or power
    * is bind or multiply


  "
  [x k]
  ;; Uses torch broadcasting: x and k must be broadcast-compatible.
  ;; Caller controls dimensions.
  (let [k-tensor (cond (number? k) k
                       (coll? k) (torch/tensor (vec k)
                                               :device *torch-device*
                                               :dtype torch/float32)
                       :else (py.. k (to :dtype torch/float32)))
        angles (torch/angle x)
        scaled-angles (torch/mul angles k-tensor)
        result (torch/complex (torch/cos scaled-angles)
                              (torch/sin scaled-angles))]
    result))

(def fpe fractional-power-encoding)

(comment
  (def a (hd/seed))
  (def b (hd/seed))
  (hd/similarity a (hd/normalize (hd/bind a b)))
  (hd/similarity (hd/bind a a) (fpe a 2))

  (hd/similarity
   (fpe a 1.5)
   [(fpe a 1.5)
    (fpe a 2.0)
    (fpe a 2.5)])

  (def x (hd/seed 2))
  (def sp (fpe x (torch/tensor [1 2 3])))

  (hd/similarity (py/get-item x [0]) sp)
  ;; tensor([[ 1.0000, -0.0041, -0.0108],
  ;;       [ 0.0011, -0.0061, -0.0140]])
  (hd/similarity (py/get-item x [1]) sp)
  ;; tensor([[ 0.0011, -0.0061, -0.0053],
  ;;       [ 1.0000, -0.0117, -0.0034]])

  (hd/similarity (fpe x 3) sp)

  (fpe
   x
   (torch/tensor [1 2 3])))
