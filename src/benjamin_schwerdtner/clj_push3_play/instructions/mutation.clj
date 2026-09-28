(ns benjamin-schwerdtner.clj-push3-play.instructions.mutation
  (:require [benjamin-schwerdtner.clj-push3-play.instructions.impl
             :refer [register-instruction] :as impl]
            [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
            [benjamin-schwerdtner.clj-push3-play.prot :as prot]
            [benjamin-schwerdtner.clj-push3-play.util :as util :refer
             [ensure-list]]
            [random-seed.core :refer [rand-int]])
  (:refer-clojure :exclude [rand rand-int]))

;; maybe 'variation' as name is more clean/ abstract and has more beautiful connotations?

;; Supposed to be the primitive instructions of Push mutation operators.

;; ---------------------------------------------------------------------------------------
;;
;; Mutation design:
;;
;;
;;
;; mutate(mutator-program, input-program, random-seed) -> variations
;;
;;
;; - `input-program` is a PUSH program.
;; - `mutator-program` is a PUSH program.
;; - `random-seed` is a number used for deterministic randomness.
;;
;;
;; mutate:
;;
;; 0. setup Push interpreter with `mutator-program` on the EXEC stack.
;; 1. Push `input-program` as list on the CODE stack.
;; 2. Set `random-seed` as special state for program execution.
;; 3. Execute program (with limits, etc.)
;; 4. Filter the CODE stack for elements of type list, called programs.
;; 5. Each such program counts as 1 potential VARIATION of `input-program`.
;;
;; - proceed by using `n-variations` variations in your EA.
;;
;; Optionally: they are sorted?
;;
;;
;; Future:
;; - Setup some kind of epigenetic register.
;; - translate Push programs in high dimensional vectors and back,
;;   allow 'neuronal' mutation on subsymbolic representations, + have a door into deep learning etc.
;;
;;
;;
;;
;;
;; ---------------------------------------------------------------------------------------

;;
;; Some design thought processes:
;;
;;
;; first, rest, drop, take
;; duplication of CODE instructions?
;;
;; solution #1: Simply use the code stack for mutator programs?
;; then the CODE instructions are already there

;; solution #2: Duplicate code instructions, whatever.
;; --------------

;; idea #1:
;; - input program goes on code stack
;;   as list?
;;   spread out?
;; - output is the content of code stack, or the top item?
;;

;; idea #2:

;; - intruduce a program input register,
;; - (an instruction MUT_INPUT, that puts the input program on the code stack) ?

;; idea #3:

;; - separate PROGRAM stack
;; - then programs that evolved to manipulate CODE don't work?

;; idea #4:

;; - do the most silly thing imaginable
;; - transform the program into bit encoded string, put it on the integer stack

;; idea #5:

;; - use PUSHP instructions (in spirit)
;; - NEIGHBOR, ELDER, OTHER, OTHER-TAG : They depend on the outer loop structure though.

;; idea #6:
;;
;; - input to the mutator program is the complete mutation program population, with weights representing something like copy numbers
;; -

;; --------------
;; #1 and #2 can be combinend

(register-instruction
 {:sym-name :mut_duplicate
  :in [:push/code]
  :out :push/code
  ;; actually the same as
  ;; code_dup code_list
  :f
  (fn [_ a]
    (if (list? a)
      (apply list (concat a a))
      (list a a)))})

;; maybe the program just needs to discover
;; length and drop

#_(register-instruction
   {:sym-name :mut_half
    :in [:push/code]
    :out :push/code
    :f
    (fn [_ code]
      (let [code (ensure-list code)]
        (apply list (take (long (/ (count code) 2)) code))))})

#_(comment
    (let [code (list 1 2 3 4)]
      (let [code (ensure-list code)]
        (apply list (take (long (/ (count code) 2)) code)))))

;; https://en.wikipedia.org/wiki/Mutation_(evolutionary_algorithm)#Mutation_of_permutations
;; (in spirit, not letter)
;;

(defn rotate
  "Returns a seq with the same length,
  where `xs` elements are move to the right `n` positions.

  Implementing a 'periodic boundary condition'.

  This is sometimes also called `roll`.
  "
  [xs n]
  (if (or (zero? n) (empty? xs))
    xs
    ;; implement periodic boundary condition
    (let [n (mod n (count xs))]
      (take (count xs) (drop n (cycle xs))))))

(comment
  (take 50 (cycle (list 1 2 3)))
  (rotate [1 2 3] 1)
  (rotate [1 2 3] 3)
  (rotate [1 2 3] 2)
  (rotate [1 2 3] 0))

(register-instruction
 {:sym-name :mut_rotate
  :in [:push/code :push/integer]
  :out :push/code
  :f
  (fn [_ code n]
    (apply list (rotate (ensure-list code) n)))})

;; -----------------------------

;; The periodic boundary condition is well-known in video games,
;; when the character moves in from the left when they move outside of the right.

;; This also called toroidal topology.

(defn inverse
  "Returns a seq with the same lenght,

  the section starting at `start` of length `len` in `xs` is reversed.

  This wraps, if start and len are outside the size of `xs`.

  Implementing a 'periodic boundary condition'.
  "
  [xs start len]
  (if (empty? xs)
    xs
    (let [xsc (cycle xs)
          cnt (count xs)
          start (mod start cnt)
          len (min cnt len)]
      (take cnt
            (concat (take start xsc)
                    (reverse (take len (drop start xsc)))
                    (drop (+ start len) xsc))))))

(register-instruction
 {:sym-name :mut_inverse
  :in [:push/code :push/integer :push/integer]
  :out :push/code
  :f
  (fn [_ code start len]
    (apply list (inverse (ensure-list code) start len)))})

;; -------------------

(comment
  (inverse [:a :b :c :d :e] 1 2)
  (inverse [:a :b :c :d :e] 1 5)
  ;; kinda has a logic, from the boundary condition
  '(:a :a :e :d :c)

  (=
   (reverse [:a :b :c :d :e])
   (inverse [:a :b :c :d :e] 5 5))
  true

  (inverse [:a :b :c :d :e] 0 2)
  '(:b :a :c :d :e))

;; -------------------------
;;
;; sex, recombination, symbiosis

(register-instruction
 {:sym-name :mut_interleave
  :in [:push/code :push/code]
  :out :push/code
  :f
  (fn [_ a b]
    (apply list (interleave (ensure-list a) (ensure-list b))))})

;; code namespace already does some:

;; code_append
;; code_s

(register-instruction
 {:sym-name :mut_drop_rand
  :in [:push/code :push/float]
  :out :push/code
  :doc "Drop elements from CODE with FLOAT chance."
  :f
  (fn [_ code prop]
    (let [code (ensure-list code)
          drop-mask (map #(< % prop)
                         (.toArray (.doubles random-seed.core/*rng*
                                             (count code))))]
      (apply list
             (map first (remove second (map vector code drop-mask))))))})

;; -------------------------
;;
;; Epigenetics: metadata hanging off the program tree.
;;
;; The marker is not in a side register keyed by some name - it is Clojure
;; metadata attached to the code object itself. That dissolves the identity
;; problem: nothing has to name a region and keep pointing at it while the
;; region moves. A subtree IS an object, and its metadata travels with it,
;; because the operators here rebuild the OUTER list and pass the elements
;; through by reference. `(1 :mut_rotate)` on a genome with a marked subtree
;; comes out the other side with the marker still on that subtree; `code_dup`
;; + `code_append` gives two copies and BOTH carry it, which is what gene
;; duplication needs.
;;
;; The analogy is tight in one more respect: `=` and `hash` ignore metadata,
;; so a marker-only change is invisible to selection. It rides along as a
;; hitchhiker, exactly like an epigenetic mark with no phenotypic effect.
;;
;; The metadata is a MAP, and a counter is one entry in it under a key like
;; :mut-count. There is deliberately no propagation policy here: whether a
;; marker survives being modified, whether a counter is carried across a
;; mutation or reset, is M's business. M is evolved, so the bookkeeping
;; discipline is evolved with it rather than designed by hand.
;;
;; Only IObj can carry metadata, so numbers and keywords cannot be marked
;; directly. A singleton list is the workaround and it is semantically free
;; in this interpreter - a list on EXEC is spliced back element by element,
;; so `(:integer_+)` runs exactly like `:integer_+` at a cost of one step.

(defn- markable?
  [x]
  (instance? clojure.lang.IObj x))

(register-instruction
 {:sym-name :code_meta_get
  :in [:push/code :push/clj-object]
  :out :state
  :doc "Read a key from the top CODE item's metadata onto the stack of its
type. The code goes back on CODE - reading a marker must not consume the
genome. Absent keys and unmarkable atoms are a NOOP, so a mutator cannot be
broken by a marker that is not there."
  :f
  (fn [state code k]
    (let [state (stack/push state :push/code code)]
      (if-let [[_ v] (and (markable? code) (find (meta code) k))]
        (stack/push state (prot/m-typeof-item v) v)
        state)))})

(register-instruction
 {:sym-name :code_meta_set
  :in [:push/code :push/clj-object :push/clj-object]
  :out :push/code
  :doc "Write a CLJ-OBJECT value into the top CODE item's metadata under the
top CLJ-OBJECT key."
  :f
  (fn [_ code k v]
    (if (markable? code)
      (vary-meta code assoc k v)
      code))})

(register-instruction
 {:sym-name :code_meta_set_int
  :in [:push/code :push/clj-object :push/integer]
  :out :push/code
  :doc "Write an INTEGER into the top CODE item's metadata under the top
CLJ-OBJECT key."
  :f
  (fn [_ code k v]
    (if (markable? code)
      (vary-meta code assoc k v)
      code))})

(register-instruction
 {:sym-name :code_meta_inc
  :in [:push/code :push/clj-object]
  :out :push/code
  :doc "Increment the counter under the top CLJ-OBJECT key in the top CODE
item's metadata, starting from 0.

Read-add-write in one instruction on purpose. A mutator that keeps a tally -
`this region was mutated N times` - has to be reachable, and requiring three
instructions in the right order to move one counter puts it out of reach of
a search that has no gradient toward bookkeeping in the first place."
  :f
  (fn [_ code k]
    (if (markable? code)
      (vary-meta code update k (fnil inc 0))
      code))})

;; -------------------------
;;
;; Reading the population: sex, one level down.
;;
;; The recombination machinery already exists - :mut_interleave, :code_append,
;; :code_subst all take two CODE items. What was missing was a second genome.
;; These instructions put one there, and every existing operator becomes
;; sexual for free.
;;
;; This is Pushpop's NEIGHBOR / ELDER / OTHER (ideas/autoconstructive.org),
;; with one difference that matters: there is no fixed two-parent crossover
;; operator anywhere. M decides whether to mate, with whom, and how. The
;; recombination policy is a genotype, so it is evolved rather than designed.
;;
;; M deliberately gets NO fitness information. It draws from a sample, not
;; from a ranking. If M could see fitness it would implement its own
;; selection, and the separation between variation and selection - the thing
;; that makes this an evolutionary algorithm at all - would collapse.
;;
;; :code_elder reads the graveyard, the genomes of individuals that have been
;; selected out. That is the "keep genomes of dead individuals around, as part
;; of the algorithm" line of ideas/metaevolution.org: a module that was fine
;; can outlive the organism that died for unrelated reasons.

(defn- draw
  [state stack-key idx]
  (let [pool (get state stack-key)]
    (if (seq pool)
      (stack/push state :push/code (nth pool (mod (abs (long idx)) (count pool))))
      state)))

(register-instruction
 {:sym-name :code_other
  :in []
  :out :state
  :doc "Push another individual of the current generation onto CODE, drawn at
random. NOOP when no population was supplied."
  :f
  (fn [state]
    (draw state :push/population (rand-int (max 1 (count (:push/population state))))))})

(register-instruction
 {:sym-name :code_other_nth
  :in [:push/integer]
  :out :state
  :doc "Push the individual at INTEGER (wrapped) onto CODE. The indexed form
lets a mutator address a partner systematically rather than by luck."
  :f
  (fn [state idx] (draw state :push/population idx))})

(register-instruction
 {:sym-name :code_elder
  :in []
  :out :state
  :doc "Push a genome from the graveyard - an individual that was selected out
- onto CODE. NOOP when the graveyard is empty."
  :f
  (fn [state]
    (draw state :push/graveyard (rand-int (max 1 (count (:push/graveyard state))))))})

;; -------------------------

(register-instruction
 {:sym-name :mut_drop_rand_one
  ;; NOTE: only CODE. `:in` args are applied to `:f` positionally
  ;; (see instructions.impl/execute-instruction), so declaring a FLOAT here
  ;; that `:f` does not take threw an ArityException on every call.
  :in [:push/code]
  :out :push/code
  :doc "Drop a random element from CODE."
  :f
  (fn [_ code]
    (let [code (ensure-list code)]
      (if (empty? code)
        code
        (let [idx (rand-int (count code))]
          (apply list (concat (take idx code) (drop (inc idx) code)))))))})
