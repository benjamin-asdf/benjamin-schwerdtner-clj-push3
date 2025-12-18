(ns benjamin-schwerdtner.clj-push3-play.hdc.fhrr
  (:require
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

;;
;; Fourier Holographic Reduced Representation
;; Proposed in "Holographic Reduced Representation: Distributed Representation for Cognitive Structures"
;; Tony A. Plate 2003
;;
;; This model uses complex phaser hypervectors.
;;

;; ------------------------------------------------------------------------------

;; ~faithfully from
;; https://github.com/hyperdimensional-computing/torchhd/blob/main/torchhd/tensors/fhrr.py

;; This software contains the following licence
;; #
;; # MIT License
;; #
;; # Copyright (c) 2023 Mike Heddes, Igor Nunes, Pere Vergés, Denis Kleyko, and Danny Abraham
;; #
;; # Permission is hereby granted, free of charge, to any person obtaining a copy
;; # of this software and associated documentation files (the "Software"), to deal
;; # in the Software without restriction, including without limitation the rights
;; # to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; # copies of the Software, and to permit persons to whom the Software is
;; # furnished to do so, subject to the following conditions:
;; #
;; # The above copyright notice and this permission notice shall be included in all
;; # copies or substantial portions of the Software.
;; #
;; # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; # IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; # FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; # AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; # LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; # OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; # SOFTWARE.
;; #

;; ---------------------------------------------------------------------------------------------

#_(def ^:dynamic *opts* {:fhrr/dimensions 512})
(def ^:dynamic *opts* {:fhrr/dimensions (long 1e4)})

(defn dimensions [] (:fhrr/dimensions *opts*))

(defn zeroes
  ([] (zeroes 1))
  ([num-vectors]
   (torch/zeros num-vectors
                (:fhrr/dimensions *opts*)
                :dtype torch/complex64
                :device *torch-device*)))

(defn unit
  "
  Returns a vector that is the neutral element of the binding operation.


  1)  ∀x;x∈H: x ⊙ `unit` = x

  2)  ∀x;x∈H: x  ⊙ inverse(x) = `unit`

  See [[inverse]].

  Alias `identity`, `ones`

  "
  ([] (unit 1))
  ([num-vectors]
   (torch/ones num-vectors
               (:fhrr/dimensions *opts*)
               :device *torch-device*
               :dtype torch/complex64)))

(def ones unit)

(defn random
  "
  Returns a fresh fhrr unit vector.

  size: the dimensions.

  "
  [size]
  (let [angle (-> (torch/empty size :dtype torch/float32 :device *torch-device*)
                  (py.. (uniform_ (- Math/PI) Math/PI)))]
    (torch/complex (py.. angle (cos)) (py.. angle (sin)))))

(defn seed
  ([] (random [(:fhrr/dimensions *opts*)]))
  ([num-vectors]
   (random
    [num-vectors (:fhrr/dimensions *opts*)])))

(defn superposition
  "Return the superposition of the hypervectors using element-wise sum.

  This produces a hypervector maximally similar to both inputs.
  The bundling operation is used to aggregate information into a single hypervector.

  Args:
    x: first hypervector
    y: second hypervector

  Returns:
    Bundled hypervector (x + y)"
  ([xs]
   (let [xs (if (coll? xs)
              (-> (torch/cat (vec xs))
                  (torch/reshape [-1 (:fhrr/dimensions *opts*)]))
              xs)]
     (torch/sum xs :dim -2)))
  ([x y] (torch/add x y)))

(def bundle superposition)

(defn bind
  "Bind hypervectors using element-wise multiplication.
  (Hadamard product).

  This produces a hypervector dissimilar to both inputs.
  Binding is used to associate information, for instance, to assign values to variables.

  Args:
    x: first hypervector
    y: second hypervector

  Returns:
    Bound hypervector (x * y)"
  ([xs]
   (let [xs (if (coll? xs)
              (-> (torch/cat (vec xs))
                  (torch/reshape [-1 (:fhrr/dimensions *opts*)]))
              xs)]
     (torch/prod xs :dim -2)))
  ([x y] (torch/mul x y)))

(defn inverse
  "Invert the hypervector for binding.

  For FHRR the inverse of a hypervector is its complex conjugate.
  This operation satisfies: x ⊙ inverse(x) = unit

  Args:
    x: hypervector to invert

  Returns:
    Inverted hypervector (complex conjugate of x)"
  [x]
  (py.. (torch/conj x) (resolve_conj)))

(defn unbind [x y] (bind x (inverse y)))

(defn negative
  "Negate the hypervector for the bundling inverse.

  This operation satisfies: x ⊕ negative(x) = zeroes

  Args:
    x: hypervector to negate

  Returns:
    Negated hypervector (-x)"
  [x]
  (torch/negative x))

(defn permute
  "Permute the hypervector by circular shifting.

  The permutation operator is used to assign an order to hypervectors.

  Args:
    x: hypervector to permute
    shifts: number of positions to shift (default: 1)

  Returns:
    Permuted hypervector"
  ([x] (permute x 1))
  ([x shifts]
   (torch/roll x shifts -1)))

(defn normalize
  "Normalize the hypervector.

  The normalization preserves the element phase but sets the magnitude to one.
  Each element becomes: z/|z| = cos(angle(z)) + i*sin(angle(z))

  Args:
    x: hypervector to normalize

  Returns:
    Normalized hypervector with unit magnitude"
  [x]
  (let [angle (torch/angle x)]
    (torch/complex (torch/cos angle) (torch/sin angle))))

(defn dot-similarity
  "Compute inner product similarity with other hypervectors.

  Args:
    x: query hypervector
    others: hypervector(s) to compare against

  Returns:
    Real-valued similarity scores"
  [x others]
  (let [others (if (>= (py.. others -ndim) 2)
                 (torch/transpose others -2 -1)
                 others)]
    (torch/real (torch/matmul x (torch/conj others)))))

(defn cosine-similarity
  "Compute cosine similarity with other hypervectors.

  Args:
    x: query hypervector
    others: hypervector(s) to compare against
    eps: small epsilon to avoid division by zero (default: 1e-8)

  Returns:
    Cosine similarity scores in range [-1, 1]"
  ([x others] (cosine-similarity x others 1e-8))
  ([x others eps]
   (let [;; Compute magnitudes
         x-dot (torch/sum (torch/real (torch/mul x (torch/conj x))) :dim -1)
         x-mag (torch/sqrt x-dot)

         others-dot (torch/sum (torch/real (torch/mul others (torch/conj others))) :dim -1)
         others-mag (torch/sqrt others-dot)

         ;; Handle broadcasting for magnitude computation
         magnitude (if (>= (py.. x -ndim) 2)
                     (torch/mul (py.. x-mag (unsqueeze -1)) (py.. others-mag (unsqueeze -2)))
                     (torch/mul x-mag others-mag))

         ;; Clamp magnitude to avoid division by zero
         magnitude (torch/clamp magnitude :min eps)]

     ;; Return cosine similarity
     (torch/div (dot-similarity x others) magnitude))))

(defn similarity
  "Dot similarity scaled to dimensions."
  [x others]
  (let [others (if (coll? others) (torch/cat (vec others)) others)]
    (torch/div
     (dot-similarity x others)
     (:fhrr/dimensions *opts*))))

;; Helper functions for convenience and API consistency

;; (defn empty
;;   "Alias for zeroes - creates hypervectors representing empty sets.
;;   When bundled with a random hypervector x, the result is x."
;;   ([] (empty {}))
;;   ([opts] (zeroes opts)))

;; (defn identity
;;   "Alias for unit - creates identity hypervectors.
;;   When bound with a random hypervector x, the result is x."
;;   ([] (identity {}))
;;   ([opts] (unit opts)))

;; Magnitude and angle functions for completeness

(defn magnitude
  "Compute the magnitude of complex hypervector elements.

  Args:
    x: hypervector

  Returns:
    Real-valued tensor with magnitudes of each element"
  [x]
  (torch/abs x))

(defn angle
  "Compute the angle/phase of complex hypervector elements.

  Args:
    x: hypervector

  Returns:
    Real-valued tensor with angles in range [-π, π]"
  [x]
  (torch/angle x))

;; ---------------------------------------------------------

(defn cleanup-idx
  ([x codebook] (cleanup-idx x codebook nil))
  ([x codebook threshold]
   (let [similarities (similarity x codebook)
         max-idx (torch/argmax similarities)]
     (if-not threshold
       max-idx
       (let [sim (py.. (py/get-item similarities max-idx) item)]
         (when (<= threshold sim) max-idx))))))

(defn cleanup
  "Clean up a hypervector by finding the closest match from a codebook.

  This is useful after unbinding operations to recover clean symbolic vectors.

  Args:
    x: noisy hypervector to clean up
    codebook: collection of clean hypervectors

  Returns:
    The hypervector from codebook with highest cosine similarity to x"
  ([x codebook] (cleanup x codebook nil))
  ([x codebook threshold]
   (let [max-idx (cleanup-idx x codebook threshold)]
     (when max-idx
       (py..
        (torch/index_select codebook 0 max-idx)
        (squeeze))))))

;; -------------------

(defn similar?
  ([a b] (similar? a b 0.95))
  ([a b threshold]
   (<= threshold (py.. (similarity a b) (squeeze) (item)))))

(comment
  (py..
   (similarity (seed) (seed))
   (squeeze)
   (item)))

;; (def ⊗ bind)
;; (def ⊘ unbind)
;; (def ⊗ superposition)
