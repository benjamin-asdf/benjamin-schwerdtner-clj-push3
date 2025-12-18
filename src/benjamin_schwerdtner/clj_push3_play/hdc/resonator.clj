(ns benjamin-schwerdtner.clj-push3-play.hdc.resonator
  (:require
   [clojure.math.combinatorics :refer [cartesian-product]]
   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]

   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]))

(require-python '[torch :as torch])
(require-python '[torch.nn.functional :as F])

;; The problem
;;
;; `factorize`, given a hd find the contributing hdvs:

;;
;; 1. Given a hd `x`.
;; 2. Given a list of codebooks, book-0, book-1, book-2, ...
;; 3. Find a hypervector for each book that together produce x, such that
;;
;;
;;           a-0 ∈ book-0, a-1 ∈ book-1, ...
;;
;; and       x = bind(a-0, a-1, ... )
;;
;;
;;
;; The reference factorizer implementation
(defn exhaustive-search-factorize
  "Returns a seq of hdvs from `books`,

  This does an exhaustive search over all possible hdv combinations.

  "
  [x books]
  ;; search space is the cartesian product of the hdvs in books
  (->> (apply cartesian-product books)
       (some (fn [hdvs]
               (when (hd/similar? x (hd/bind hdvs)) hdvs)))))

(comment
  ;; "Elapsed time: 67.839989 msecs"
  (time (let
         [books [(hd/seed 10)
                 (hd/seed 10)
                 (hd/seed 10)]
          [a b c] (mapv (fn [b]
                          (py/get-item b (rand-int 10)))
                        books)
          x (hd/bind [a b c])

          [af bf cf]
          (into [] (exhaustive-search-factorize x books))]

          (hd/similarity
           (torch/stack [a b c])
           (torch/stack [af bf cf]))))

  ;; tensor([[ 1.0000, -0.0020, -0.0066],
  ;;         [-0.0020,  1.0000,  0.0034],
  ;;         [-0.0066,  0.0034,  1.0000]])
  )

;; -------------------------------

;; 1.
;;
;; Resonator networks for factoring distributed representations of data structures
;; Frady, Kent, Olshausen, Sommer 2020
;; Neural Computation 32, 2311-2331 (2020)
;;
;; https://arxiv.org/abs/2007.03748

;; 2.
;;
;; https://arxiv.org/abs/2208.12880v4

;; --------------------------------------
;;
;; The resonator factorizer algorithm, can be interpreted as a kind of recurrent neural net
;; 'resonator network'.
;;

;; - uses computing in superposition,
;; - iterates on estimates, in parallel
;; - solutions emerge as stable fixed-points in the network dynamics
;;

;; Assume you have:
;;
;; Eqn (1)    s = xi ⊙ yj ⊙ zk
;;
;; Where xi, yj, zk are drawn from codebooks
;;
;; X = { x1, ..., xD }, etc.
;;
;; I call `D` the codebook resolution.
;; e.g.
;; D = 100

;;
;; The resonator combines superposition and cleanup
;;

;; -----------------------------
;; A FHRR resonator is found at
;;
;; https://arxiv.org/abs/2208.12880v4

;;
;; reference impl
;; -  (not FHRR) https://github.com/spencerkent/resonator-networks/blob/master/resonator_networks/dynamics/rn_numpy.py#L7
;; -  https://codeocean.com/capsule/1211768/tree/v1
;;

;; -----------------------------

(defn norm
  [x]
  (torch/div (torch/norm x)
             (torch/mul (torch/sqrt (torch/tensor (py.. x (size 0))))
                        2)))

(defn norm-abs [x] (torch/div x (torch/abs x)))

(def resonator-defaults
  {:phasor-projection
     ;; set the mag of all vector elements to 1
   (fn [x] (torch/div x (norm x)))
   :non-linearity
     ;; Quote:
     ;; Our experiments show that a combination of a ReLU and
     ;; polynomial exponent performs best
   (fn poly [x {:keys [cleanup-k]}]
       ;; the k paramater controls the amount of superposition of
       ;; different possible solutions, i.e. the sparsity
       ;;
       ;; below 1: weaken the cleanup (k=0 would return the
       ;; complete superposition)
       ;; above 1: achieve a stricter cleanup like argmax, or
       ;; winner-take-all for high ks.
     (torch/pow (F/relu x) cleanup-k))
   ;; cleanup-k > 1 didn't do much for me.
   :cleanup-k 1
   :noise-level 0.001
   :update-ratio 0.1
   :max-iterations 13
   :similarity-threshold 0.9})

(defn resonator-step-fhrr
  "Returns new estimates for factorizing `target`, performs a resonator step with `books`.

  shapes:

  - target: (d)
  - books: (n, m, d)
  - estimates: (n, d)
  - output: (n, d)

  d: hdv dimension
  n: books/ factors count
  m: book resolution

  Each new estimate is given by the equivalent of this (for x):

  eqn 1:  x-estimate(t + 1) = g(X^T p(X(s ⊙ y-estimate(t)^-1 ⊙ z-estimate(t)^-1))) + η


  ⊙    : bind
  ^-1    : inverse
  s      : the input / target (to be factorized)
  p(x)   : a a non-linearity
  g(x)   : a phasor-projection, setting the mag of the vectors 1
  η      : a random noise to help prevent local optima

  So for each factor,

  - unbind with the 'other factor' estimates, yielding an estimate of the factor.
  - perform a cleanup with the given book, using the outer matrix product of the book.
  - This step is conceptually similar to a Hopfield Net.
  - Normalize outputs with an activation function `g`, prevent run away effects.

  --

  Beyond the update described, we only change the next state by an update-ratio factor γ:

  if eqn 1 is 'update', then

  x(t+1) = γ * update(x) +  (1 - γ) * x

  So with low `update-ratio`, we mostly keep x, just like a residuals net in deep learning;
  Where each layer only contributes an update but does not replace the output.
  "
  [target books estimates
   {:keys [ ;; p(x)
           non-linearity
           ;; η
           noise-level
           ;; g(x)
           phasor-projection
           ;; γ
           update-ratio]
    :as opts}]
  (let [estimates (torch/unsqueeze estimates 0)
        n (py.. estimates (size -2))
        ;; ----------------------
        ;;
        ;; 1. For each factor, find the
        ;; other factors and bind them
        others (-> (reduce (fn [rolled i]
                             (into rolled
                                   (torch/roll estimates i -2)))
                           []
                           (range 1 n))
                   ;; shape: (n,n-1,d)
                   (torch/stack :dim -2)
                   ;; shape: (n,d)
                   (hd/bind))
        ;; -----------------------------
        ;;
        ;; 2.
        ;; e = s ⊙ others^-1
        ;; shape: (n,d)
        new-estimate (hd/bind target (hd/inverse others))
        ;; -----------------------------
        ;;
        ;; 3.
        ;; Cleanup / autoassociative part. Scale the output
        ;; pattern by it's similarity (activation).
        ;;
        ;; attn = e*X^
        ;; Like address decoder neuron activations
        ;; shape: (n)
        ;;
        attn
        (torch/real (torch/einsum "nd,nmd->nm"
                                  new-estimate
                                  (torch/resolve_conj (torch/conj
                                                       books))))
        ;; 4.
        ;; update attention
        ;;
        ;; 4a
        ;; Optionally bias
        ;; att (torch/mul attn bias)
        ;;
        ;; 4b
        ;; Optional non linearity
        ;; p(x)
        ;;
        attn (non-linearity attn opts)
        ;; recover pattern using attention
        attn (py.. attn (type torch/cfloat))
        patt-update (torch/einsum "nm,nmd->nd" attn books)
        ;; ----------------------
        ;; 5. update by γ
        pattern (torch/add (torch/mul update-ratio patt-update)
                           (torch/mul (- 1 update-ratio) estimates))
        ;;
        ;; pattern = ~ XX^T(e)
        ;; ----------------------------
        ;;
        ;; 6.
        ;; out = g(pattern)
        outputs (phasor-projection pattern)
        ;; 7. + noise
        outputs (if (nil? noise-level)
                  outputs
                  (torch/add outputs
                             (torch/mul (torch/randn_like outputs)
                                        noise-level)))]
    (torch/squeeze outputs)))

(defn resonator-fhrr
  "perform a resonator factorization of `x` with `books`.
  returns an outcome info map with the following keys:

  `:factors`: a seq of hdvs, one for each book. binding the together yields something similar to `x`.
  `:finish-reason`: a keyword indicating why the factorization finished.
                    can be `:factorized` or `:max-iterations-reached`.
  `:success?`: true if the factorization was successful, false otherwise.


  `n`: factors count
  `m`: book resolution
  `d`: hdv dimension

  shapes:

  books: (n, m, d)
  x: (d)

  opts:

  `similarity-threshold`: sim as by [[hd/similarity]] for which estimates are accepted.
                          depends on how dirty your input is.

  `max-iterations`: number of iterations before giving up.


  + resonator options

  see [[resonator-defaults]]  , [[resonator-step-fhrr]]


  "
  ([x books] (resonator-fhrr x books resonator-defaults))
  ([x books {:keys [max-iterations similarity-threshold] :as opts}]
   (let [{:keys [max-iterations similarity-threshold] :as opts}
         (merge resonator-defaults opts)
         initial-estimates (hd/superposition books)
         finish-info
         (fn [{:keys [n-iteration estimates]}]
           (cond (hd/similar? x
                              (hd/normalize (hd/bind estimates))
                              similarity-threshold)
                 (let [factor-idxs (mapv (fn [x book]
                                           (hd/cleanup-idx x
                                                           book))
                                         estimates
                                         books)]
                   {:factor-idxs factor-idxs
                    :factors (into []
                                   (map (fn [idx b]
                                          (py/get-item
                                           b
                                           (py.. idx item)))
                                        factor-idxs
                                        books))
                    :finish-reason :factorized
                    :success? true})
                 (< max-iterations n-iteration)
                 {:finish-reason :max-iterations-reached
                  :success? false}
                 :else nil))]
     (loop [{:keys [n-iteration estimates] :as state}
            {:estimates initial-estimates :n-iteration 0}]
       ;; ------------------------
       (if-let [info (finish-info state)]
         (merge state info)
         (let [estimates (resonator-step-fhrr x books estimates opts)]
           (recur {:estimates estimates
                   :n-iteration (inc n-iteration)})))))))

(def factorize resonator-fhrr)

(comment

  (do
    (def books (hd/random [3 10 (hd/dimensions)]))
    (def a (py/get-item books [0 -1]))
    (def b (py/get-item books [1 -1]))
    (def c (py/get-item books [2 -1]))
    (def x (hd/bind [a b c]))
    (factorize x books)))
