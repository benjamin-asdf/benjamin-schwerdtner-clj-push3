(ns benjamin-schwerdtner.metabiology.mutation
  "Algorithmic mutation: the mutation operator is itself a program.

  This is Chaitin's central move, and the one that makes metabiology different
  from a genetic algorithm. A mutation M is a program. Applying it to organism A
  gives the child

      A' = M(A)

  and the probability of drawing M is 2^-|M|: exponentially unlikely in the
  *size of the program that describes the change*, not in the number of symbols
  the change touches.

  The consequence, which is the whole reason to bother:

      Reversing the genome, or doubling it, is one algorithmic step. Under
      point mutation it is exponentially unlikely (2^-k for k symbols).
      Hamming distance is maximal; algorithmic distance is 1.

  So the reachable neighbourhood of an organism is not a ball of small edits.
  It is every transformation with a short description. Gradualism is not
  assumed. This is the sense in which the mutation distance from A to B is the
  relative information content of B given A (Chaitin 1975) - approximated here
  by [[mutation-distance]].

  ------------------------------------------------------------------------
  How a mutator runs
  ------------------------------------------------------------------------

  A mutator is a Push3 program run with the parent organism sitting on the CODE
  stack. Every list it leaves on CODE counts as one candidate child.

  The run itself is [[benjamin-schwerdtner.clj-push3-play.mutation.mut]] - the
  shared `mutate(E, M)` substrate - which absorbs the exceptions evolved code
  throws, catches its stack overflows, seeds each application separately, and
  narrows the insertion alphabet to the target domain. See [[variants]] for what
  that buys and for the one thing it does not fix.

  What this namespace adds on top is the metabiology-specific part: the
  organism-alphabet insertion instructions, the algorithmic mutation
  distribution, and mutation distance.

  ------------------------------------------------------------------------
  Where we deviate from Chaitin
  ------------------------------------------------------------------------

  Chaitin's 2^-|M| lives on self-delimiting binary programs, where the Kraft
  inequality makes the weights sum to <= 1 and the distribution is honest. Push
  programs over an A-symbol alphabet are not self-delimiting: there are A^k
  programs of size k, so weighting each by 2^-k gives total mass (A/2)^k, which
  diverges.

  So we sample in two stages: draw a size k with P(k) proportional to
  2^(-k/`:complexity-halving`), then draw uniformly among programs of that size.
  What is preserved is the property the theory actually leans on - probability
  decaying exponentially in description length, so that short radical
  transformations are common and long specific ones are exponentially rare. What
  is lost is the exact numerical weight. `:complexity-halving` is how many extra
  code points it takes to halve a mutation's probability."
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require
   [benjamin-schwerdtner.clj-push3-play.mutation.mut :as mut]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.instructions.impl
    :refer [register-instruction]]
   [benjamin-schwerdtner.metabiology.organism :as organism]
   [random-seed.core :refer [rand rand-int rand-nth]]))

;; ---------------------------------------------------------------------------
;; Where new material comes from
;;
;; A mutation that can only rearrange what is already there cannot get an
;; organism off the ground from '(). Something has to splice in novel code.
;;
;; :code_rand is the obvious candidate and it is the wrong one: it draws from
;; `gen/default-generators` concatenated with the identifier list, and since
;; that is 4 junk generators (booleans, random ints, gensyms, empty
;; collections) against 1 identifier generator, ~80% of everything it splices
;; in is inert. Organisms bloat with gensyms and nils while learning nothing.
;;
;; These two instructions are the same idea drawing from the organism alphabet
;; only.
;; ---------------------------------------------------------------------------

(def ^:private generators-for
  "Atom generators for whatever alphabet the run narrowed the state to.

  `mut/setup` puts the caller's `:alphabet` into the state as `:instructions`,
  so reading it here is what makes new material belong to the organism being
  evolved. These two instructions used to close over `organism/atom-generators`
  directly, which meant :mb_rand_atom spliced BIGNUM instructions into every
  genome whatever landscape the regimes were pointed at - invisible in the
  busy-beaver runs, where that is the right alphabet, and fatal anywhere else.

  Memoised because the alphabet is fixed for a run and this is called once per
  inserted atom. Behaviour for metabiology is unchanged: `organism/atom-generators`
  is itself `(gen/identifier-generators organism/alphabet)`, and `variants`
  defaults `:alphabet` to that same vector."
  (memoize (fn [instructions] (gen/identifier-generators (vec instructions)))))

(defn- new-material-generators
  [state]
  (if-let [instrs (seq (:instructions state))]
    (generators-for instrs)
    organism/atom-generators))

(register-instruction
 {:sym-name :mb_rand_code
  :in [:push/integer]
  :out :push/code
  :doc "Pushes a newly generated random *organism* program onto CODE. Size limit
  from INTEGER."
  :f (fn [state limit]
       (gen/generate-code
        {:atom-generators (new-material-generators state)
         :branch-probability (constantly 0.15)
         :close-probability (constantly 0.0)
         :max-points (inc (abs (mod limit 12)))}))})

(register-instruction
 {:sym-name :mb_rand_atom
  :in []
  :out :push/code
  :doc "Pushes one random atom of the organism alphabet onto CODE."
  :f (fn [state] (gen/rand-atom (new-material-generators state)))})

;; ---------------------------------------------------------------------------
;; The mutator alphabet
;;
;; CODE instructions (the parent arrives on the CODE stack) plus the mut_*
;; primitives, which are exactly the "one algorithmic step, exponentially
;; unlikely bitwise" operators: duplicate the genome, reverse a stretch of it,
;; rotate it, interleave two genomes (sex / symbiosis), drop random pieces.
;; ---------------------------------------------------------------------------

(def mutator-alphabet
  (into (mapv (fn [i] i) (range 0 6))
        [;; whole-genome transformations - the point of the exercise
         :mut_duplicate
         :mut_inverse
         :mut_rotate
         :mut_interleave
         :mut_drop_rand
         :mut_drop_rand_one
         ;; structural surgery
         :code_append
         :code_cons
         :code_car
         :code_cdr
         :code_list
         :code_insert
         :code_extract
         :code_subst
         :code_nthcdr
         :code_length
         ;; new material (organism alphabet - see above)
         :mb_rand_code
         :mb_rand_atom
         ;; plumbing
         :code_dup
         :code_swap
         :code_rot
         :code_quote
         :integer_+
         :integer_dup
         :float_rand]))

(def atom-generators (gen/identifier-generators mutator-alphabet))

(def default-opts
  ;; min 2 points: a 1-point mutator is always the empty program, i.e. a
  ;; guaranteed neutral mutation and a wasted evaluation.
  {:min-points 2
   :max-points 24
   ;; What :mb_rand_atom / :code_rand_atom insert. The organism alphabet
   ;; unless the caller is evolving something else.
   :alphabet organism/alphabet
   ;; extra code points needed to halve a mutation's probability
   :complexity-halving 3
   ;; a mutator is a program too: it gets a step budget
   :max-executions 2000
   ;; refuse children that have bloated past this - a resource bound on us,
   ;; not a claim about biology
   :max-organism-points 400})

;; ---------------------------------------------------------------------------
;; The algorithmic mutation distribution
;; ---------------------------------------------------------------------------

(defn size-distribution
  "Sizes paired with their (unnormalised) weights 2^(-k/halving)."
  [{:keys [min-points max-points complexity-halving]}]
  (mapv (fn [k]
          [k (Math/pow 2.0 (- (/ (double k) (double complexity-halving))))])
        (range min-points (inc max-points))))

(defn sample-size
  [opts]
  (let [dist (size-distribution opts)
        total (reduce + (map second dist))
        target (* (rand) total)]
    (loop [[[k w] & more] dist
           acc 0.0]
      (let [acc (+ acc w)]
        (if (or (nil? more) (<= target acc))
          k
          (recur more acc))))))

(defn log2-probability
  "log2 of the probability of drawing `mutator`, up to the normalising constant.

  This is the quantity Chaitin writes as -|M|: the exponent in 2^-|M|. Reported
  in code points scaled by `:complexity-halving` so that it lines up with the
  distribution actually being sampled."
  ([mutator] (log2-probability mutator {}))
  ([mutator opts]
   (let [{:keys [complexity-halving]} (merge default-opts opts)]
     (- (/ (double (organism/size mutator)) (double complexity-halving))))))

(defn random-mutator
  "Draws a mutation program from the algorithmic mutation distribution."
  ([] (random-mutator {}))
  ([opts]
   (let [opts (merge default-opts opts)]
     (gen/generate-code {:max-points (sample-size opts)
                         :atom-generators atom-generators
                         :branch-probability (constantly 0.1)
                         :close-probability (constantly 0.0)}))))

;; ---------------------------------------------------------------------------
;; Applying a mutation
;; ---------------------------------------------------------------------------

(defn variants
  "Every candidate child `mutator` produces from `parent`, top of CODE first.

  Delegates the actual run to [[mut/mutate]], which is the shared substrate for
  `mutate(E, M)` (see ideas/metaevolution.org). Two things it gives us that are
  not optional for an evolutionary run:

  - evolved mutators call instructions on empty stacks, coerce nonsense and
    recurse without a base case. `mut/run` absorbs exceptions as NOOPs and
    catches StackOverflowError, so a bad mutator costs one offspring instead of
    the whole run.
  - `:alphabet` narrows `:instructions` to the organism alphabet, so a mutator
    that inserts a random atom inserts organism material.

  Note `:alphabet` does not fully solve insertion: :code_rand and
  :code_rand_atom concatenate `gen/default-generators` (booleans, gensyms,
  empty collections) with the identifier generator at 4:1, so ~80% of what they
  splice is inert whatever the alphabet is. That is why this namespace still
  registers :mb_rand_code / :mb_rand_atom above.

  Children larger than `:max-organism-points` are dropped."
  ([parent mutator] (variants parent mutator {}))
  ([parent mutator opts]
   (let [{:keys [max-executions max-organism-points alphabet]}
         (merge default-opts opts)]
     (into []
           (filter (fn [p] (<= (organism/size p) max-organism-points)))
           (mut/mutate parent
                       mutator
                       ;; a fresh seed per application, drawn from the run's
                       ;; `*rng*`, so repeated applications of one stochastic
                       ;; mutator are genuinely different draws and the run as a
                       ;; whole stays reproducible from its seed
                       {:seed (rand-int Integer/MAX_VALUE)
                        ;; The ORGANISM's alphabet, which is the busy-beaver
                        ;; one by default but is handed in when the regimes
                        ;; are pointed at a different landscape - see
                        ;; problems/propeller/metabiology.clj. Hard-coding it
                        ;; meant :mb_rand_atom inserted bignum instructions
                        ;; into a program-synthesis genome.
                        :alphabet alphabet
                        :max-executions max-executions})))))

(defn mutate
  "A' = M(A). Returns the child, or nil if `mutator` produced nothing usable.

  A mutator that leaves the parent untouched on CODE is a neutral mutation, and
  correctly returns the parent."
  ([parent mutator] (mutate parent mutator {}))
  ([parent mutator opts]
   (first (variants parent mutator opts))))

;; ---------------------------------------------------------------------------
;; Mutation distance = relative information content
;; ---------------------------------------------------------------------------

(defn mutation-distance
  "Upper bound on the algorithmic mutation distance from `a` to `b`.

  Searches mutators in increasing size for one with M(a) = b, and returns
  {:points k :mutator M} for the smallest found, or nil if the search budget
  ran out. That k is an upper bound on the program-size complexity of the
  simplest M with b = M(a) - Chaitin's relative information content of b given
  a, which is not computable, so an upper bound by bounded search is the best
  anyone gets.

  `:tries-per-size` is how many random mutators of each size to try."
  ([a b] (mutation-distance a b {}))
  ([a b {:as opts :keys [max-points tries-per-size]
         :or {max-points 8 tries-per-size 2000}}]
   (let [opts (merge default-opts opts)]
     (some (fn [k]
             (some (fn [_]
                     (let [m (gen/generate-code
                              {:max-points k
                               :atom-generators atom-generators
                               :branch-probability (constantly 0.1)
                               :close-probability (constantly 0.0)})]
                       (when (= b (mutate a m opts))
                         {:points (organism/size m) :mutator m})))
                   (range tries-per-size)))
           (range 1 (inc max-points))))))

(comment
  (require '[benjamin-schwerdtner.metabiology.mutation :as mut] :reload)

  ;; The headline claim, run. Doubling the genome is ONE algorithmic step:
  (mut/mutate '(2 :bignum_frominteger) '(:mut_duplicate))
  ;; => (2 :bignum_frominteger 2 :bignum_frominteger)
  ;; Hamming distance: the whole genome. Algorithmic distance: 1.

  ;; Reversing a stretch, also one step:
  (mut/mutate '(1 2 3 4 5) '(9 0 :mut_inverse))

  (mut/random-mutator)
  (frequencies (repeatedly 200 #(count (mut/random-mutator))))

  ;; What does it cost to get from A to B?
  (mut/mutation-distance '(2 :bignum_frominteger)
                         '(2 :bignum_frominteger 2 :bignum_frominteger)))
