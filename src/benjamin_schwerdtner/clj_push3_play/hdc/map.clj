(ns benjamin-schwerdtner.clj-push3-play.hdc.map
  (:require
   [libpython-clj2.require :refer [require-python]]
   [libpython-clj2.python :refer [py. py..] :as py]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]))

(require-python '[torch :as torch]
                '[torch.nn.functional :as F])

;;
;; Multiply Add Permute (MAP)
;; Proposed in "Multiplicative Binding, Representation Operators & Analogy"
;; https://www.researchgate.net/publication/215992330_Multiplicative_Binding_Representation_Operators_Analogy
;;
;; This model works with dense bipolar hypervectors with elements from {-1, 1}.
;;

;; ------------------------------------------------------------------------------

;; ~faithfully from
;; https://github.com/hyperdimensional-computing/torchhd/blob/main/torchhd/tensors/map.py

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

(def ^:dynamic *opts* {:map/dimensions (long 1e4)})

(defn empty
  "Creates a set of hypervectors representing empty sets.
  
  When bundled with a hypervector x, the result is x.
  
  Args:
    num-vectors: number of hypervectors to generate (default: 1)
  
  Returns:
    Zero-filled hypervector(s) of the specified dimension"
  ([] (empty 1))
  ([num-vectors]
   (torch/zeros num-vectors
                (:map/dimensions *opts*)
                :dtype torch/float32
                :device *torch-device*)))

(defn identity
  "Creates a set of identity hypervectors.
  
  When bound with a random hypervector x, the result is x.
  
  Args:
    num-vectors: number of hypervectors to generate (default: 1)
  
  Returns:
    Hypervector(s) filled with ones"
  ([] (identity 1))
  ([num-vectors]
   (torch/ones num-vectors
               (:map/dimensions *opts*)
               :dtype torch/float32
               :device *torch-device*)))

(defn random
  "Creates a set of random independent hypervectors.
  
  The resulting hypervectors are sampled uniformly at random from the 
  dimensions-dimensional hyperspace with elements from {-1, 1}.
  
  Args:
    num-vectors: number of hypervectors to generate (default: 1)
  
  Returns:
    Random bipolar hypervector(s) with elements in {-1, 1}"
  ([] (random 1))
  ([num-vectors]
   (let [size [num-vectors (:map/dimensions *opts*)]
         select (-> (torch/empty size :dtype torch/bool :device *torch-device*)
                    (py.. (bernoulli_)))]
     (torch/where select -1.0 1.0))))

(def seed random)

(defn bundle
  "Bundle hypervectors using element-wise sum.
  
  This produces a hypervector maximally similar to both.
  The bundling operation is used to aggregate information into a single hypervector.
  
  Args:
    When called with 1 arg:
      xs: collection of hypervectors or tensor with multiple hypervectors
    When called with 2 args:
      x: first hypervector
      y: second hypervector
  
  Returns:
    Bundled hypervector (sum of inputs)"
  ([xs]
   (let [xs (if (coll? xs)
              (-> (torch/cat (vec xs))
                  (torch/reshape [-1 (:map/dimensions *opts*)]))
              xs)]
     (torch/sum xs :dim -2 :dtype torch/float32)))
  ([x y] (torch/add x y)))

(def superposition bundle)
(def multibundle bundle)

(defn bind
  "Bind hypervectors using element-wise multiplication.
  
  This produces a hypervector dissimilar to both.
  Binding is used to associate information, for instance, to assign values to variables.
  
  Args:
    When called with 1 arg:
      xs: collection of hypervectors or tensor with multiple hypervectors
    When called with 2 args:
      x: first hypervector
      y: second hypervector
  
  Returns:
    Bound hypervector (element-wise product)"
  ([xs]
   (let [xs (if (coll? xs)
              (-> (torch/cat (vec xs))
                  (torch/reshape [-1 (:map/dimensions *opts*)]))
              xs)]
     (torch/prod xs :dim -2 :dtype torch/float32)))
  ([x y] (torch/mul x y)))

(def multibind bind)

(defn inverse
  "Invert the hypervector for binding.
  
  Each hypervector in MAP is its own inverse, so this returns a copy of self.
  This operation satisfies: x ⊙ inverse(x) = identity
  
  Args:
    x: hypervector to invert
  
  Returns:
    A copy of the input hypervector"
  [x]
  (torch/clone x))

(defn unbind
  "Unbind y from x.
  
  Since each MAP vector is its own inverse, this is equivalent to bind(x, y).
  
  Args:
    x: first hypervector
    y: second hypervector
  
  Returns:
    x bound with the inverse of y"
  [x y]
  (bind x (inverse y)))

(defn negative
  "Negate the hypervector for the bundling inverse.
  
  This operation satisfies: x ⊕ negative(x) = empty
  
  Args:
    x: hypervector to negate
  
  Returns:
    Negated hypervector (-x)"
  [x]
  (torch/negative x))

(defn permute
  "Permute the hypervector by circular shifting.
  
  The permutation operator is commonly used to assign an order to hypervectors.
  
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
  
  The normalization sets all positive entries to +1 and all other entries to -1.
  
  Args:
    x: hypervector to normalize
  
  Returns:
    Normalized bipolar hypervector with elements in {-1, 1}"
  [x]
  (let [positive (torch/tensor 1.0 :dtype torch/float32 :device *torch-device*)
        negative (torch/tensor -1.0 :dtype torch/float32 :device *torch-device*)]
    (torch/where (py.. x (gt 0)) positive negative)))

(defn clipping
  "Performs the clipping function that clips the lower and upper values.
  
  Args:
    x: hypervector to clip
    kappa: specifies the range of the clipping function [-kappa, kappa]
  
  Returns:
    Clipped hypervector with values in range [-kappa, kappa]"
  [x kappa]
  (torch/clamp x :min (- kappa) :max kappa))

(defn dot-similarity
  "Compute inner product similarity with other hypervectors.
  
  Args:
    x: query hypervector
    others: hypervector(s) to compare against
  
  Returns:
    Inner product similarity scores"
  [x others]
  (let [others (if (>= (py.. others -ndim) 2)
                 (torch/transpose others -2 -1)
                 others)]
    (torch/matmul x others)))

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
         x-dot (torch/sum (torch/mul x x) :dim -1 :dtype torch/float32)
         x-mag (torch/sqrt x-dot)

         others-dot (torch/sum (torch/mul others others) :dim -1 :dtype torch/float32)
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
  "Dot similarity scaled to dimensions.
  
  Args:
    x: query hypervector
    others: hypervector(s) to compare against
  
  Returns:
    Scaled similarity scores"
  [x others]
  (let [others (if (coll? others) (torch/cat (vec others)) others)]
    (torch/div
     (dot-similarity x others)
     (:map/dimensions *opts*))))

;; Helper functions for cleanup and matching

(defn cleanup-idx
  "Find the index of the closest matching hypervector in a codebook.
  
  Args:
    x: query hypervector
    codebook: collection of clean hypervectors
    threshold: optional similarity threshold
  
  Returns:
    Index of best match, or nil if threshold not met"
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
  
  This is useful after bundling/unbinding operations to recover clean symbolic vectors.
  
  Args:
    x: noisy hypervector to clean up
    codebook: collection of clean hypervectors
    threshold: optional similarity threshold
  
  Returns:
    The hypervector from codebook with highest similarity to x"
  ([x codebook] (cleanup x codebook nil))
  ([x codebook threshold]
   (let [max-idx (cleanup-idx x codebook threshold)]
     (when max-idx
       (py..
        (torch/index_select codebook 0 max-idx)
        (squeeze))))))

(defn similar?
  "Check if two hypervectors are similar above a threshold.
  
  Args:
    a: first hypervector
    b: second hypervector
    threshold: similarity threshold (default: 0.95)
  
  Returns:
    Boolean indicating if similarity is above threshold"
  ([a b] (similar? a b 0.95))
  ([a b threshold]
   (<= threshold (py.. (similarity a b) (squeeze) (item)))))

;; Convenience functions
(def zeroes empty)
(def ones identity)

(comment
  ;; Test basic operations
  (binding [*opts* {:map/dimensions 1000}]
    (let [a (random)
          b (random)
          c (random)]

      ;; Test identity property: x ⊙ identity = x
      (similar? (bind a (identity)) a)

      ;; Test empty property: x ⊕ empty = x  
      (similar? (bundle a (empty)) a)

      ;; Test inverse property: x ⊙ inverse(x) = identity
      (similar? (bind a (inverse a)) (identity))

      ;; Test negative property: x ⊕ negative(x) = empty
      (let [result (bundle a (negative a))]
        (py.. (normalize result) (squeeze) (item)))

      ;; Test bundling distributes over binding
      ;; (a ⊕ b) ⊙ c ≈ (a ⊙ c) ⊕ (b ⊙ c)
      (let [left (bind (bundle a b) c)
            right (bundle (bind a c) (bind b c))]
        (similar? left right 0.9)))))
