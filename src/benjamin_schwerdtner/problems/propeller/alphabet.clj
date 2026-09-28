(ns benjamin-schwerdtner.problems.propeller.alphabet
  "Which instructions a problem is allowed to use.

  Every Propeller problem defines its instruction set the same way:

      (concat (get-stack-instructions #{:exec :integer :boolean :string :print})
              (list :in1)
              (list 'close)
              (list \"\" \"small\" \"large\" random-int))

  - a set of STACKS, expanded to every instruction that touches only those,
  - the problem's INPUT instructions,
  - and its ERCs, the literals and random-literal generators.

  That shape is the problem definition as much as the error function is: it is
  what makes `small-or-large` a problem about integers and strings and not
  about vectors of floats. So it is reproduced here rather than replaced by
  \"all instructions\".

  Two things do not carry over.

  `close` is Propeller's genome marker: a Propeller genome is a flat sequence
  (a `plushy`) and `close` is where a nesting level ends, so the linear genome
  can encode a tree. Genomes here are already trees - `generator/generate-code`
  emits nested lists directly and the CODE instructions in
  `instructions/code.clj` operate on that structure - so there is nothing for
  `close` to mark. Where Propeller tunes `close` frequency, this port tunes
  `:branch-probability`.

  Instruction NAMES differ, because this implementation had its own before the
  port: `:integer_+` not `:integer_add`, `:integer_stackdepth` not
  `:integer_stack_depth`, `:integer_fromboolean` not `:integer_from_boolean`.
  See [[propeller-name]] for the mapping - it exists so a Propeller program
  found in a paper can be read into this system, not because anything here
  needs it."
  (:require
   [clojure.string :as string]
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :as impl]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as instructions]))

;; ---------------------------------------------------------------------------
;; Which stacks does an instruction touch?
;;
;; Propeller carries this as `:stacks` metadata on every instruction. This
;; implementation does not, so it is derived: an instruction's `:in` and `:out`
;; name their stacks directly, and what remains is the `:out :state` family,
;; where the extra stacks follow from the name. The derivation is checked at
;; the bottom of this namespace rather than trusted.
;; ---------------------------------------------------------------------------

(def input-pseudo-stack
  "The stack `:in1` .. are treated as touching.

  Not a real stack - the inputs live in `(:push/input state)`. It exists so
  that the input instructions are never pulled in by a stack set, which is
  also how Propeller handles them: every problem lists its `:inN` explicitly,
  because which inputs exist is a property of the problem and not of the
  stacks it computes over."
  :push/input)

(def output-register-pseudo-stack
  "The stack `:set_c0` .. and `:c0` .. are treated as touching.

  Like [[input-pseudo-stack]], not a real stack, and for the same reason: how
  many output registers exist is a property of the PROBLEM - six for the
  3-bit multiplier, eight for the 4-bit one - so a problem lists the ones it
  has explicitly rather than having them pulled in by asking for BOOLEAN."
  :push/output)

(def random-pseudo-stack
  "The stack a stochastic instruction is treated as touching.

  Propeller's `get-stack-instructions` says it \"won't include random
  instructions unless :random is in the set as well\", and that exclusion is
  load-bearing rather than tidiness. A stochastic instruction in the alphabet
  means the same genome scores differently on the same case each time it is
  evaluated, so:

  - error is noise, and lexicase selects the individual that got lucky on the
    shuffled case rather than the one that is better;
  - the fitness memoisation in `mutation/dual.clj` becomes wrong, because it
    keys on the program and assumes the score is a function of it.

  Measured before this was here: `integer-regression`, whose stack set is just
  `#{:exec :integer}`, drew `:integer_rand` into its alphabet and no run could
  fit a deterministic cubic."
  :push/random)

(def ^:private vector-elem-types
  [:push/boolean :push/char :push/float :push/integer :push/string])

(defn- vector-type-of-name
  "`\"vector_integer_pushall\"` -> `[:push/vector :push/integer]`."
  [n]
  (some (fn [elem]
          (when (string/starts-with? n (str "vector_" (name elem) "_"))
            [:push/vector elem]))
        vector-elem-types))

(defn- prefix-type
  "The stack an instruction's name says it belongs to."
  [n]
  (or (vector-type-of-name n)
      (some (fn [t]
              (when (string/starts-with? n (str (name t) "_")) t))
            [:push/boolean :push/bignum :push/char :push/clj-object :push/code
             :push/exec :push/float :push/integer :push/name :push/string
             :push/print :push/grid :push/task :push/ca-rule])))

(defn- extra-stacks
  "Stacks an `:out :state` instruction touches that its `:in` does not name.

  Everything here is a consequence of the instruction's own implementation, so
  it is a lookup table and not a heuristic - the alternative is adding a
  `:stacks` key to five hundred registrations."
  [sym-name]
  (let [n (name sym-name)
        vec-type (vector-type-of-name n)
        elem (second vec-type)]
    (cond-> #{}
      ;; The count-driven duplicators read n off INTEGER without declaring it
      ;; in `:in`, because they have to inspect the stack before committing.
      (some #(string/ends-with? n %) ["_dup_times" "_dup_items" "_deep_dup"])
      (conj :push/integer)

      ;; The output channel.
      (or (string/ends-with? n "_print") (= n "print_newline"))
      (conj :push/print)

      ;; A vector spills its elements onto the element stack.
      (and elem (string/ends-with? n "_pushall")) (conj elem)
      ;; ... and iterating hands each element to a body taken from EXEC.
      (and elem (string/ends-with? n "_iterate")) (conj elem :push/exec)

      (= n "string_iterate") (conj :push/char :push/exec)
      (#{"string_parse_to_chars" "char_all_from_string"} n) (conj :push/char)

      ;; CODE instructions run what they build, so they reach EXEC.
      (string/starts-with? n "code_") (conj :push/exec)
      ;; The mutation operators of mutation/mut.clj rewrite the CODE stack.
      (string/starts-with? n "mut_") (conj :push/code)

      (#{"name_quote" "name_randboundname"} n) (conj :push/name)

      (re-matches #"in\d+" n) (conj input-pseudo-stack)

      ;; The output registers. `:set_cN` declares BOOLEAN in its `:in`; `:cN`
      ;; declares nothing at all, because whether it pushes depends on
      ;; whether the register has been written.
      (re-matches #"(set_)?c\d+" n)
      (conj output-register-pseudo-stack :push/boolean)

      ;; Anything stochastic, by name. See [[random-pseudo-stack]].
      (re-find #"rand" n) (conj random-pseudo-stack))))

(defn stacks-of
  "The set of stacks `sym-name` reads or writes."
  [sym-name]
  (let [{:push.instruction/keys [in out]} (impl/instruction sym-name)]
    (into (cond-> (set (remove #{:state} (conj (vec in) out)))
            (prefix-type (name sym-name)) (conj (prefix-type (name sym-name))))
          (extra-stacks sym-name))))

(def stack-instruction-index
  "sym-name -> set of stacks, for every registered instruction."
  (delay
    (into {} (map (juxt identity stacks-of)) (instructions/all-instructions))))

(defn get-stack-instructions
  "Every instruction that touches only stacks in `stacks`.

  Propeller's `get-stack-instructions`. `stacks` may be given in Propeller's
  short form (`#{:integer :vector_integer}`) or in this implementation's
  (`#{:push/integer [:push/vector :push/integer]}`)."
  [stacks]
  (let [stacks (into #{} (map (fn [s]
                                (cond
                                  (vector? s) s
                                  (= "push" (namespace s)) s
                                  (string/starts-with? (name s) "vector_")
                                  [:push/vector
                                   (keyword "push"
                                            (subs (name s)
                                                  (count "vector_")))]
                                  :else (keyword "push" (name s)))))
                     stacks)]
    (into []
          (comp (filter (fn [[_ ss]] (and (seq ss)
                                          (every? stacks ss))))
                (map key))
          (sort-by (comp name key) @stack-instruction-index))))

;; ---------------------------------------------------------------------------
;; ERCs
;; ---------------------------------------------------------------------------

(defn atom-generators
  "The 0-arg generators `generator/generate-code` draws a genome's atoms from.

  Propeller's instruction list is a flat sequence mixing instruction keywords,
  literal constants and 0-arg random-literal functions, and a genome atom is
  drawn uniformly from it. One generator per element reproduces that
  distribution exactly - which matters, because it is what sets how often a
  random program mentions the input versus a constant versus an instruction."
  [alphabet]
  (mapv (fn [a] (if (fn? a) a (constantly a))) alphabet))

;; ---------------------------------------------------------------------------
;; Reading Propeller's instruction names
;; ---------------------------------------------------------------------------

(def propeller-name->ours
  "Propeller instruction name -> this implementation's, where they differ.

  Only the irregular ones. The regular rewritings - `_stack_depth` ->
  `_stackdepth`, `_yank_dup` -> `_yankdup`, `_from_x` -> `_fromx` on the
  numeric stacks - are handled by [[propeller-name]]."
  {:integer_add :integer_+
   :integer_subtract :integer_-
   :integer_mult :integer_*
   :integer_eq :integer_=
   :integer_gt :integer_>
   :integer_lt :integer_<
   :integer_gte :integer_>=
   :integer_lte :integer_<=
   :float_add :float_+
   :float_subtract :float_-
   :float_mult :float_*
   :float_eq :float_=
   :float_gt :float_>
   :float_lt :float_<
   :float_gte :float_>=
   :float_lte :float_<=
   :boolean_eq :boolean_=
   :string_eq :string_=
   :char_eq :char_=
   :code_eq :code_=
   :exec_eq :exec_=
   :string_occurrencesof_char :string_occurences_char
   :string_empty :string_empty?
   :exec_do_count :exec_do*count
   :exec_do_range :exec_do*range
   :exec_do_times :exec_do*times})

(def semantics-differ
  "Instructions that exist in both systems and do NOT mean the same thing.

  Renaming is not enough for these - a Propeller program using one of them
  computes something else here, so [[propeller-name]] alone will not port it.

  Both are operand ORDER on the multi-string operators, where Push3 specifies
  nothing and the two implementations picked opposite conventions:

    :string_contains  Propeller asks whether the SECOND string contains the
                      top one; here, whether the TOP contains the second.
    :string_replace   Propeller's target string is the DEEPEST of the three
                      and the replacement is on top; here it is the reverse.

  Left as they are rather than changed to match. Nothing in Push3 or in these
  instructions' own documentation makes either convention the correct one, and
  a search process only needs the semantics to be CONSISTENT - it will find
  whichever order it is given. The arithmetic operators were a different case
  and were fixed: `:integer_-`, `:integer_mod` and `:integer_div` contradicted
  both the Push3 specification and their own docstrings."
  #{:string_contains :string_replace})

(defn propeller-name
  "This implementation's name for a Propeller instruction keyword, or nil.

  For reading a published Push program into this interpreter. Returns nil when
  no such instruction exists here, so a caller can see what is missing rather
  than silently dropping it."
  [k]
  (let [candidate
        (or (propeller-name->ours k)
            (let [n (-> (name k)
                        (string/replace "_stack_depth" "_stackdepth")
                        (string/replace "_yank_dup" "_yankdup")
                        (string/replace #"_eq$" "_=")
                        ;; `:set-c5` -> `:set_c5`. The output registers are
                        ;; the only Propeller instructions spelled with a
                        ;; hyphen, because there they are defined inside the
                        ;; multiplier problem files rather than in the
                        ;; instruction set proper.
                        (string/replace "-" "_")
                        (string/replace #"^(integer|float|boolean|code)_from_"
                                        "$1_from"))]
              (keyword n)))]
    (when-not (= :no-instruction (impl/instruction candidate))
      candidate)))

;; ---------------------------------------------------------------------------
;; Checking the derivation above, at load time.
;;
;; The point of failing here rather than in a test is that a newly registered
;; instruction with a name this namespace does not recognise would otherwise
;; get an empty stack set and be quietly excluded from every problem's
;; alphabet - a change that makes problems harder for no visible reason.
;; ---------------------------------------------------------------------------

(def unclassified
  (delay (into [] (comp (filter (fn [[_ ss]] (empty? ss))) (map key))
               @stack-instruction-index)))

(when (seq @unclassified)
  (binding [*out* *err*]
    (println "WARNING propeller/alphabet: no stacks derived for"
             (pr-str @unclassified))))

(comment
  ;; What each problem family's stack set actually expands to. Worth an eyeball
  ;; after touching [[extra-stacks]].
  (count (get-stack-instructions #{:exec :integer :boolean :string :print}))
  (get-stack-instructions #{:exec :integer :boolean :vector_integer :print})
  (stacks-of :integer_print)
  (stacks-of :vector_integer_pushall)
  (stacks-of :string_iterate))
