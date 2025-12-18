(ns benjamin-schwerdtner.clj-push3-play.hdc.conceptual-hyperspace
  (:require
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.data :as hdd]
   [benjamin-schwerdtner.clj-push3-play.hdc.spatial-semantic-pointer :as ssp]
   [benjamin-schwerdtner.clj-push3-play.hdc.config :refer [*torch-device*]]

   [benjamin-schwerdtner.clj-push3-play.hdc.resonator :as resonator]
   [benjamin-schwerdtner.clj-push3-play.hdc.fractional-power-encoding :as fpe]

   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]))

;;
;; =================================================================
;;
;; Conceptual Hyperpsace (CH) is a CST implemented using HDC.
;;
;; https://arxiv.org/abs/2411.08684
;;
;; CST: Conceptual Spaces Theory
;; https://en.wikipedia.org/wiki/Conceptual_space
;;
;; =================================================================
;;

;;
;; 'Analogies are partial similarities between different situations
;; that support further inferences' [Gentner 1998].
;;
;; Eqn 1)   A : B :: C : X
;;

;; Analogy desiderata:
;; -------------------------------

;; retrieval
;; -----------
;; Given C find A and B

;; mapping
;; -----------
;; determine the structural correspondance (frame) between A and B
;; find X by applying it to C.
;;
;; A : B  is doing a lot of work... 👈
;;
;; called semantic relationships
;; theories: "category based", "property based", others: event-based, part-whole, causal
;;
;;

;; inference
;; ---------------
;; Use A to advance the concept C

;;
;; CST:
;;
;; - points denote objects
;; - regions is a concept
;;
;; HDC is a natural fit to model CST!
;;
;;
;; crucial insight: that the HDC framework could operationalize the
;; cognitive science theory of Conceptual Spaces [Gärdenfors 2000]
;;

;; ----------------------------------------------------------------
;; ---
;;
;; Domain `D`:
;; A `k`-dimensional space (low dimensional)
;;
;; for example the color domain
;;
;; k = 3
;; k-bases = { hue, saturation, brightness }
;;
;; concept: a point in D.
;;
;; e.g. point-a, point-b, point-c
;; point-a ~= red
;; point-b ~= purple, etc.
;;

;; let's stay with color example.
;; k = 3: hue, saturation, brightness

(defn bases-codebooks
  "Returns codebook-lookup info containing `k` codebooks, each with an array of fractionol powers.

  `k`: Codebook count
  `low`: The fractional power range start.
  `high`: dito end
  `resolution`: The fractional power step size.

  `basis-seeds`: Optinally 3 hypervedtors that serve as the basis for the codebooks.

  codebook-resolution = (count (range low (inc high) resolution))

  Produces books with shape [k,codebook-resolution,d]

  where d is the hyperdimensions.


  example:

  k    = 3
  low  = -10
  high = 10
  resolution = 0.1

   book 1:
   basis: x

   +----------------------------------------------------------+
   |  +-------------+                                         |
   |  | - -- ---    | x^-10                                   |
   |  |             | x^-9.9                                  |
   |  |             | ...                                     |
   |  |             | x^0                                     |
   |  |             | x^0.1                                   |
   |  |             | ...                                     |
   |  |             |                                         |
   |  |             | x^(~11)                                 |
   |  +-------------+                                         |
   +----------------------------------------------------------+
          book-1,      ...                            book-k


  returns:
   :values
     a tensor containing the fractional powers used

     range:  [low,low+resolution,low+resolution*2,...high+1]


  "
  [{:keys [k resolution high low basis-seeds]}]
  (let [;; eps to include the highest value, currently not needed
        ;; because of the inc.
        epsilon 1e-5
        ;; not sure why, but the corner of the book never got
        ;; factorized. As if by a off-by-one error but I didn't
        ;; figure out where.
        high (+ high epsilon 1)
        values
        (torch/arange low high resolution :device *torch-device*)
        basis-seeds (or basis-seeds (hd/seed k))]
    {:basis-seeds basis-seeds
     :books (fpe/fpe basis-seeds values)
     :resolution resolution
     :values values}))

(defn encode-point
  "Returns a hdv representing the concept `P` given by domain point `p`.

  `p`: A real valued vector representing a low dimensional point in the concept domain `D`.

  `bases`: A sequence of basis vectors.
   You need `k` basis vectors for the encoding of the k-dimensional domain `D`.

   dim(D) == dim(p) == count(bases) == k

   Example concept encoding for `purple`:

   PURPLE = x^6.2 ⊙ y^-6.2 ⊙ z^5.3

    k == 3, k-bases = { hue, saturation, brightness }
  "
  [p bases]
  (hd/bind (map (fn [b v] (fpe/fpe b v)) bases p)))

(defn encode
  "Returns a hdv representing the concept `P` given by domain point `p`.

  `p`: A real valued vector representing a low dimensional point in the concept domain `D`.

  Like [[encode-point]] but takes the return value of [[bases-codebooks]] for convinience.
  "
  [p {:keys [bases]}]
  (encode-point p bases))

;; -----------------------
;; Category based analogy
;; -----------------------

;; Eqn 5: pX = pC - pA + pB
;;
;; for
;;
;; PURPLE : BLUE :: ORANGE : X
;;
;; Eqn 6: x = (orange ⊗ purple^-1)  ⊗ blue
;;

;;
;; Algorithm 3 find (using parallelogram method)
;; 1: Input: a, b, c
;; 2: Output: x
;; 3: x ← (c ⊛ a−1 ) ⊛ b
;; 4: return x
;;
;; direct translation:
;; (hd/bind (hd/unbind c a) b)
;;
(defn categorical-mapping
  "
  Returns a hypervector that is the query of outcome of categorical analogical mapping:

  A : B :: C : X

  Given A, B, C find X

  a, b and c are hypervectors

  Assumes that the concepts arre in the same catergoy, e.g. color.

  PURPLE : BLUE :: ORANGE : YELLOW

  Using Parallelogram model [Rumelhart and Abrahamson, 1973].
  "
  [a b c]
  (hd/bind [(hd/inverse a) b c]))

;;
;; with x in hand, decode using a resonator network
;;

(defn decode
  "Returns a concept point, when found, in the domain D given a hypervector x.

  Second arg is like the return of [[bases-codebooks]].
  This returns nil, when factorizaiton fails.
  In that case the `x` is outside of the domain of codebooks,
  or the factorization problem is outside of the operational capacity of the resonator.

  k=3, m=50 is within capacity.


  shapes:

  x: (d)
  values: (r)
  books: (k,m,d)

  k: Domain dimensions = D
  m: book resolution
  d: hyperdim

  "
  [x {:keys [books values]}]
  (when-let [factors-idxs (:factor-idxs (resonator/factorize x books))]
    (->
     (torch/index_select values -1 (torch/tensor factors-idxs))
     (torch/round :decimals 2))))

(comment
  (do
    (def p-purple [6.2 -6.2 5.3])
    (def p-blue [0 -10 5])
    (def p-orange [6.7 5.7 10])
    (def p-yellow  [0.6 1.8 9.7])
    (def B
      (bases-codebooks
       {:high 10
        :k 3
        :low -10
        :resolution 0.1}))
    (let [[purple blue orange yellow]
          (mapv
           #(encode-point % (:basis-seeds B))
           [p-purple p-blue p-orange p-yellow])
          x
          (categorical-mapping purple blue orange)]
      [p-yellow
       (decode x B)
       (py.. (hd/similarity x yellow) item)]))
  ;;
  ;; [[0.6 1.8 9.7]
  ;;  tensor([0.5000, 1.9000, 9.7000])   <-  outcome is not as close to yellow as it could be ?
  ;;  0.9674046635627747]
  ;;
  ;; ------------
  ;;
  ;; PURPLE : BLUE :: ORANGE : X
  ;;
  ;; Q: find x.
  ;;
  ;; A: 'yellow is to orange like blue is to purple'
  ;;
  ;; ------------
  )
;; Solving a Property-based Analogy

;; compare semantic relationship between categories and properties

;; A : B :: C : X

;; property based example:

;; APPLE : RED :: BANANA : X

;; 1. identify property dimensions that correspond to concept or property B
;;    RED, color domain
;; 2. find the location within that dimension closest to prototype location C.
;;    Would be color domain dimensions for BANANA (i.e. YELLOW)
;;

;; -> this can be done with a record represenation
;; is a Dollar in Mexico situation

;; ------------
;; It is interesting to consider this generatively, if we find a point X,
;; but no concept in long term memory corresponds to X, we are free to take this
;; occurance as prototype of a fresh symbol X.
;; We can imagine that Hofstadter or Minsky analogy maturation processes
;; are implemented in terms of such moves.
;;
(defn property-based-mapping
  "
  Finds the analogical element of `prop` in `b`, given `a`.
  `a` and `b` are record-like hdvs, `prop` is either a key or a value
  in the domain of the record.


  Example:  APPLE : RED :: BANANA : X

  (property-based-mapping apple red banana)

  output: YELLOW

  Observe this is the same as

  'what is the dollar in mexico?'.
  "
  [a prop b]
  ;; second, use the key to query record b
  #_(hd/unbind b
               ;; first, find the key associated with prop in record a
               (hd/unbind a prop))
  ;; this reduces to this:
  #_(hd/bind [b (hd/inverse a) prop])
  ;; ..
  ;; it is the same as [[categorical-mapping]]
  (hd/bind [(hd/inverse a) prop b]))

(comment
  ;;
  ;;  The example merely follows it's own internal consistency.
  ;;  BANANA is a made up symbol for convinience. We imagine we
  ;;  had a cognitive system where a mature concept BANANA
  ;;  comes either from long term or perception.
  ;;
  (let [red (hd/seed)
        color-key (hd/seed)
        yellow (hd/seed)
        shape-key (hd/seed)
        round (hd/seed)
        banana-shaped (hd/seed)
        apple (hd/superposition [(hd/bind color-key red)
                                 (hd/bind shape-key round)])
        banana (hd/superposition [(hd/bind color-key yellow)
                                  (hd/bind shape-key banana-shaped)])]
    ;; apple : red :: banana : X
    ;; X is yellow
    (hd/similarity yellow (property-based-mapping apple red banana))))

;; ---------------------------

(comment
  (for [res (range 0.1 1 0.1)]
    (do (def B (bases-codebooks {:high 10 :k 3 :low -10 :resolution res}))
        (def b
          (hd/bind [(fpe/fpe (first (:basis-seeds B)) 10.0)
                    (fpe/fpe (second (:basis-seeds B)) 2.2)
                    (fpe/fpe (first (rest (rest (:basis-seeds B)))) 9.0)]))
        [res (vec (py.. (:books B) (size)))
         (:success? (resonator/factorize b (:books B)))
         (vec (py.. (decode b B) (tolist)))]))

  '([0.1 [3 211 10000] true [10.0 2.200000047683716 9.0]]
    [0.2 [3 106 10000] true [10.0 2.200000047683716 9.0]]
    [0.30000000000000004 [3 71 10000] true
     [10.100000381469727 2.299999952316284 8.90000057220459]]
    [0.4 [3 53 10000] true [10.0 2.0 9.199999809265137]]
    [0.5 [3 43 10000] true [10.0 2.0 9.0]]
    [0.6 [3 36 10000] true [9.800000190734863 2.0 9.199999809265137]]
    [0.7 [3 31 10000] true
     [10.300000190734863 1.899999976158142 8.899999618530273]]
    [0.7999999999999999 [3 27 10000] true [10.0 2.0 9.199999809265137]]
    [0.8999999999999999 [3 24 10000] true
     [9.800000190734863 2.6000001430511475 8.899999618530273]]
    [0.9999999999999999 [3 22 10000] true [10.0 2.0 9.0]]))
