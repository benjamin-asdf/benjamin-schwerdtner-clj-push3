;; -------

;; cleanup step
;;
;; (neuronal ensemble to be cleaned up)
;; address                                            output register
;; +---------------+                                 +----------------+
;; |               |                                 |                |
;; +---------------+                                 +----------------+
;;                 d                                                  d
;;        |                                                ^
;;        |                                                |
;;        |                                                |             superpose activated ensembles
;;        |                                                |
;;        v                                                |
;; +---------------+      +-----+                    +----------------+
;; |               |      | 0.5 |                    |                |  <- row: stored ensemble
;; |               |      | 0.9 |                    +----------------+
;; |               |      | 0.0 |                    |                |
;; |               +----> |     |  --------------->  +----------------+
;; |               |      |     |                    |                |
;; |               |      |     |                    |                |
;; +---------------+      +-----+                    +----------------+
;;                       activations
;;
;;  X^T:  (n, d)  * a =   act (n)          *   X (d, n)  = output (d)
;;
;;
;;  address decoder                                stored patterns
;;
;;
;; You can interpret this:
;;
;; X^T: List of address decoder neurons `(n,d)`
;; Each of the `n` addr. decoder neuron has `d` synapses
;; Input ensemble `a` activates synapses of the neuron ( `synapse-act[ij] = a_i * X^T_ij` ), sum the activation for each neuron
;; yieling `act` (n).
;;





































(defn resonator
  "A step of the resonator network that factorizes the input.

  Given current estimates for each factor, it returns the next estimates for those factors.

  Args:
    input: The hypervector to be factorized
    estimates: The current estimates of the factors (shape: [n, d])
    domains: The domains of each factor containing all possible factors (shape: [n, m, d])

  Returns:
    Next estimates for the factors (shape: [n, d])
  "
  [input estimates domains]
  (let [n (py.. estimates (size 0))     ; number of factors

        ;; Get binding inverse of the estimates
        inv-estimates (torch/stack
                       (vec (for [i (range n)]
                              (hd/inverse (py/get-item estimates i)))))

        ;; Roll over the number of estimates to align each row with the other symbols
        ;; For factorizing x, y, z the stacked matrix has the following estimates:
        ;; [[z^-1, y^-1],
        ;;  [x^-1, z^-1],
        ;;  [y^-1, x^-1]]
        rolled (torch/stack
                (vec (for [i (range 1 n)]
                       (torch/roll inv-estimates i -2))))

        ;; First bind all the other estimates together
        inv-others (torch/prod rolled :dim -2)

        ;; Then unbind all other estimates from the input
        new-estimates (torch/mul (py.. input (unsqueeze -2)) inv-others)

        ;; Compute similarity with domains
        similarity (hd/dot-similarity (py.. new-estimates (unsqueeze -2)) domains)

        ;; Weighted sum of domain vectors based on similarities
        output (torch/squeeze
                (hd/dot-similarity similarity (torch/transpose domains -2 -1))
                -2)]

    ;; Normalize the output vector with a non-linearity (sign function)
    (torch/sgn output)))

(defn resonator-factorize
  "Factorize a hypervector using the resonator network algorithm.

  Given a hypervector and domains, iteratively refines estimates until convergence.

  Args:
    x: The hypervector to be factorized
    books: The domains of each factor containing all possible factors
           Can be a collection of tensors or a stacked tensor

  Returns:
    The factors that compose x, or nil if no solution found
  "
  [x books]
  (let [;; Convert books to stacked tensor if needed
        books (if (coll? books)
                (torch/stack (vec books))
                books)
        n (py.. books (size 0)) ; number of factors

        ;; Initial estimates: superposition of all possible vectors in each domain
        initial-estimates (torch/stack
                           (vec (for [i (range n)]
                                  (hd/normalize (hd/superposition* (py/get-item books i))))))

        ;; Convergence criteria - check if estimates factorize to x
        converged? (fn [estimates]
                     (let [;; Apply permutations based on position
                           permuted (torch/stack
                                     (vec (for [i (range n)]
                                            (hd/permute (py/get-item estimates i) i))))
                          ;; Bind all together
                           product (torch/prod permuted :dim -2)]
                       (> (torch/item (hd/similarity x product)) 0.95)))

        ;; Maximum iterations to prevent infinite loops
        max-iterations 100]

    (loop [estimates initial-estimates
           iteration 0]
      (if (or (converged? estimates) (>= iteration max-iterations))
        ;; Return the final estimates after cleanup to closest domain vectors
        (vec (for [i (range n)]
               (let [estimate (py/get-item estimates i)
                     book (py/get-item books i)
                     similarities (hd/dot-similarity estimate book)
                     best-idx (torch/item (torch/argmax similarities))]
                 (py/get-item book best-idx))))

        ;; Perform resonator step
        (let [new-estimates (resonator x estimates books)]
          (recur new-estimates (inc iteration)))))))
