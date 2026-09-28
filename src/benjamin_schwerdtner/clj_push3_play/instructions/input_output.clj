(ns benjamin-schwerdtner.clj-push3-play.instructions.input-output
  "INPUT and PRINT instructions.

  These are the two channels a program-synthesis benchmark needs and that the
  original Push3 description does not have, because Push3 was specified for
  evolving programs that leave their answer on a stack. The PSB1/PSB2 suites
  are specified the other way round: a case is a named tuple of inputs, and the
  answer is what the program *printed*.

  INPUT. `:in1` .. `:in8` read `(:push/input state)` and push the value onto the
  EXEC stack. Pushing onto EXEC rather than onto the value's own stack is
  Propeller's choice and it is the right one: the interpreter's literal
  dispatch then puts it where it belongs, so one instruction handles integers,
  strings, vectors and everything else without knowing the problem's types.

  The important property is that an input is READABLE MORE THAN ONCE. With the
  input merely pre-loaded onto a stack - which is what
  `problems/propeller_adapter.clj` did - a program that needs `x` twice has to
  have found `:integer_dup` first, and one that consumes it in the wrong order
  can never recover it. Most of the PSB problems are unsolvable in that regime
  for reasons that have nothing to do with the problem.

  An `:inN` with no matching key NOOPs rather than throwing. Propeller throws,
  which is safe there because its instruction set per problem is exactly the
  inputs that exist. Here a mutation operator can splice in any identifier from
  the alphabet, and an exception would count as a program failure rather than
  as the no-op it should be - which is the same discipline as every other
  instruction in this implementation, where a starved stack means NOOP.

  PRINT. `:push/print` is an accumulator, not really a stack: its top item is
  the output produced so far, and `<type>_print` pops a value and appends its
  printed form. It starts holding `\"\"` (see `interpreter/setup-state`), so a
  program that prints nothing has printed the empty string rather than
  producing `:no-stack-item`. Numbers and booleans go through `pr-str`, strings
  and chars through `str` - so printing the string `\"ab\"` yields `ab` and not
  `\\\"ab\\\"`. That is Propeller's rule and the benchmark's expected outputs
  are written against it."
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction]]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

;; ---------------------------------------------------------------------------
;; INPUT
;; ---------------------------------------------------------------------------

(def max-inputs
  "How many `:inN` instructions exist. The widest PSB1/PSB2 problem is `grade`
  with five inputs; eight leaves room without registering instructions no
  problem can use."
  8)

(def input-instructions
  (mapv #(keyword (str "in" %)) (range 1 (inc max-inputs))))

(doseq [sym-name input-instructions]
  (register-instruction
   {:sym-name sym-name
    :in []
    :out :state
    :doc "Pushes this case's input value onto the EXEC stack."
    :f (fn [state]
         (let [input (:push/input state)]
           (if (contains? input sym-name)
             (stack/push state :push/exec (get input sym-name))
             state)))}))

(defn with-inputs
  "`state` with the case inputs bound, so that `:in1` .. can read them.

  Takes either a map (`{:in1 3 :in2 \"x\"}`) or a sequential collection, which
  is numbered from 1 - the common case, since a benchmark case is a tuple."
  [state inputs]
  (assoc state
         :push/input
         (if (map? inputs)
           inputs
           (into {}
                 (map-indexed (fn [i v] [(keyword (str "in" (inc i))) v]))
                 inputs))))

;; ---------------------------------------------------------------------------
;; PRINT
;; ---------------------------------------------------------------------------

(defn printed
  "What `state` has printed so far."
  [state]
  (let [top (stack/peek-item state :push/print)]
    (if (= top :no-stack-item) "" top)))

(defn- print-form
  "How a value appears when printed. Strings and chars print as themselves,
  everything else through `pr-str`."
  [item]
  (if (or (string? item) (char? item))
    (str item)
    (pr-str item)))

(defn- append-print
  [state s]
  (let [current (printed state)]
    (-> state
        ;; `pop` on an empty vector throws, and the PRINT accumulator is empty
        ;; whenever a state was not built by `setup-state`.
        (cond-> (seq (get-in state [:stacks :push/print]))
          (stack/pop-item :push/print))
        (stack/push :push/print (str current s)))))

(register-instruction
 {:sym-name :print_newline
  :in []
  :out :state
  :doc "Appends a newline to the printed output."
  :f (fn [state] (append-print state \newline))})

(def printable-types
  [:push/boolean :push/char :push/code :push/exec :push/float :push/integer
   :push/string
   [:push/vector :push/boolean]
   [:push/vector :push/char]
   [:push/vector :push/float]
   [:push/vector :push/integer]
   [:push/vector :push/string]])

(defn type->prefix
  "The instruction-name prefix for a push type: `:push/integer` -> \"integer\",
  `[:push/vector :push/integer]` -> \"vector_integer\"."
  [t]
  (if (vector? t)
    (str "vector_" (name (second t)))
    (name t)))

(doseq [t printable-types]
  (register-instruction
   {:sym-name (keyword (str (type->prefix t) "_print"))
    :in [t]
    :out :state
    :doc "Pops a value and appends its printed form to the output."
    :f (fn [state item] (append-print state (print-form item)))}))

;; ---------------------------------------------------------------------------
;; OUTPUT REGISTERS
;;
;; `:set_c0` .. `:set_c7` write the top BOOLEAN into a register; `:c0` .. `:c7`
;; read it back. Eight of them because the widest benchmark that uses them is
;; the 4-bit multiplier, whose product needs eight bits.
;;
;; Why a register bank rather than just reading the BOOLEAN stack: a circuit
;; has several independent outputs, and the order a program computes them in
;; is its own business. Scoring the top six items of the BOOLEAN stack would
;; force a program to produce its bits in one particular order and to leave
;; nothing else behind - which is a constraint on the SOLUTION, not part of
;; the problem. Registers let a program set c0 first and c5 last.
;;
;; Reading a register back matters as much as writing it. Sub-expressions
;; recur across the bits of a multiplier - the same partial products appear in
;; several output bits - so `:cN` is how a program reuses one it has already
;; computed instead of rebuilding it.
;;
;; Propeller spells these `:set-c5` / `:c5` and defines them inside each
;; multiplier problem file; here they are ordinary instructions, and
;; `alphabet/propeller-name` maps the hyphenated spelling onto these.
;; ---------------------------------------------------------------------------

(def n-output-registers 8)

(doseq [i (range n-output-registers)]
  (let [reg (keyword (str "c" i))]
    (register-instruction
     {:sym-name (keyword (str "set_c" i))
      :in [:push/boolean]
      :out :state
      :doc "Pops a BOOLEAN into this output register."
      :f (fn [state v] (assoc-in state [:push/output reg] v))})
    (register-instruction
     {:sym-name reg
      :in []
      :out :state
      :doc "Pushes this output register onto BOOLEAN, if it has been set."
      :f (fn [state]
           (let [v (get-in state [:push/output reg])]
             ;; An unset register is not `false` - it is nothing. Pushing
             ;; `false` would hand the program a value it never computed and
             ;; make an unset register indistinguishable from a deliberate
             ;; zero, which is exactly the distinction the error function
             ;; scores.
             (if (boolean? v)
               (stack/push state :push/boolean v)
               state)))})))

(defn output-registers
  "The values of registers `c0` .. `c(n-1)`, most significant FIRST.

  MSB first because that is how the multiplier benchmarks write their expected
  answer - `[c5 c4 c3 c2 c1 c0]` - so the two line up positionally. Unset
  registers come back as `:unset`, which no boolean equals."
  [state n]
  (mapv (fn [i] (get-in state [:push/output (keyword (str "c" i))] :unset))
        (reverse (range n))))
