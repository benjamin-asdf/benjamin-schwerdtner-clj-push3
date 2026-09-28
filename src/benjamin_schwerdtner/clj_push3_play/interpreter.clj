(ns benjamin-schwerdtner.clj-push3-play.interpreter
  (:require
   [random-seed.core :refer [random *rng*]]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   ;; [benjamin-schwerdtner.clj-push3-play.stack.state :as s-st]
   [benjamin-schwerdtner.clj-push3-play.prot :as prot]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as
    instructions]
   [benjamin-schwerdtner.clj-push3-play.configuration.config :as config]))

;; -
;; https://faculty.hampshire.edu/lspector/push3-description.html
;; -

;; =============================
;; Push3
;; =============================

(defn typeof-item
  "Returns the type of the item, as a keyword."
  [item]
  (prot/m-typeof-item item))

(defn push-to-stack [state item]
  (stack/push state (typeof-item item) item))

(defn name? [item]
  (= :push/name (typeof-item item)))

(defn literal? [p]
  (not (#{:push/name :push/code} (typeof-item p))))

(defn handle-name
  "Handle a push identifier.
  "
  [state identifier]
  (->
   ;; When the name-quoted flag is set, a identifier is pushed onto the NAME stack
   (if (::name-quoted? state)
     (stack/push state :push/name identifier)
     ;; NOTE this was `(get state :bindings identifier)`, which reads the key
     ;; :bindings out of the STATE with the identifier as the default - so it
     ;; returned the whole bindings map, which is non-nil even when empty.
     ;; Every name therefore took the bound branch and pushed the bindings map
     ;; itself onto EXEC, and no name ever reached the NAME stack. That made
     ;; the NAME stack and hence every *_define instruction unreachable.
     (if-let [v (get (:bindings state) identifier)]
       ;; the bound value is pushed to the exec stack
       (stack/push state :push/exec v)
       ;; if the identifier was not encountered before, push it on the NAME stack
       (stack/push state :push/name identifier)))
   (assoc ::name-quoted? false)))

(instructions/register-instruction
 {:sym-name :name_quote
  :in []
  :out :state
  :doc "The next name is pushed on the NAME stack, regardless of whether it is bound or not.
Presumably for redefinition."
  :f (fn [state] (assoc state ::name-quoted? true))})

;; using the Push EXEC stack for recursion, inside Push

(defn execute-load
  [state program]
  (stack/push state :push/exec program))

(defn execute-1
  [state]
  ;; NOTE: `*rng*` is bound once per [[execute]] call, not here. Re-seeding on
  ;; every step made the stochastic instructions (code_rand, integer_rand,
  ;; mut_drop_rand ...) return the same draw at every step, i.e. constants.
  (if (empty? (-> state
                  :stacks
                  :push/exec))
    state
    (let [item (stack/peek-item state :push/exec)
          state (stack/pop-item state :push/exec)
            ;; every exec pop call is +1 :push/num-executions
          state (update state :push/num-executions (fnil inc 0))
          instruction (instructions/instruction item)]
      (cond (not= instruction :no-instruction)
            (instructions/execute-instruction state instruction)
            (name? item) (handle-name state item)
            ;; A NAMED INPUT. `:in1` .. are registered instructions, but a
            ;; problem may name its inputs instead - the boolean multiplier
            ;; benchmarks use `:a2 :a1 :a0 :b2 :b1 :b0`, where the name says
            ;; which bit it is. Any keyword present in the input map resolves
            ;; to its value, which is Propeller's design
            ;; (`handle-input-instruction`) and means a problem can introduce
            ;; input names without registering instructions for them.
            ;;
            ;; Checked after the instruction table, so it can never shadow a
            ;; real instruction, and before `literal?`, which would otherwise
            ;; push the keyword itself onto CLJ-OBJECT.
            (and (keyword? item) (contains? (:push/input state) item))
            (stack/push state :push/exec (get (:push/input state) item))
            (literal? item) (push-to-stack state item)
              ;; item is a list
            :else (reduce (fn [state p]
                            (stack/push state :push/exec p))
                          state
                            ;; reverse, so the first item in
                            ;; the list is the first exec
                            ;; instruction
                          (reverse item))))))

(defn execute
  "Entry point for Push3 execution.

  `program`: A Push3 program. A list of Push3 instructions and literals.

  `state`: presumably from [[setup-state]].


  Opts:

  `max-executions`: Stop executions after this count of EXEC stack pops.


  Randomness: the stochastic instructions draw from `*rng*`, which is seeded
  once per call from `:push/random-seed` in `state` (default 0). One binding for
  the whole run, so the stream advances from step to step while the run as a
  whole stays reproducible from its seed.

  ---------

  https://faculty.hampshire.edu/lspector/push3-description.html

  "

  ([state program] (execute state program {}))
  ([state program {:keys [max-executions]}]
   (binding [*rng* (random (:push/random-seed state 0))]
     (let [state (execute-load state program)]
       (loop [state (assoc state :push/num-executions 0)]
         (if (or (when max-executions
                   (<= max-executions (:push/num-executions state)))
                 (empty? (-> state
                             :stacks
                             :push/exec)))
           state
           (recur (execute-1 state))))))))

;; ---

(defn setup-state
  []
  (merge
   (config/defaults)
   {:bindings {}
    :instructions (instructions/all-instructions)
    ;; What `:in1` .. read. Empty unless a problem bound it, see
    ;; instructions/input_output.clj.
    :push/input {}
    ;; Output REGISTERS, written with `:set_cN` and read back with `:cN`. A
    ;; second output channel next to PRINT, and a different shape: PRINT is an
    ;; append-only string, this is addressable storage. The boolean circuit
    ;; benchmarks need it because they have six or eight independent outputs
    ;; that a program may compute in any order.
    :push/output {}
    :stacks
    {:push/boolean []
     :push/char []
     :push/clj-object []
     :push/code []
     :push/exec []
     :push/float []
     :push/integer []
     ;; Unbounded integers: the busy-beaver / metabiology output channel.
     :push/bignum []
     :push/name []
     ;; The output channel. Its top item is everything printed so far, and it
     ;; starts at "" rather than empty so that "printed nothing" is a value a
     ;; program can be scored on instead of a missing stack item.
     :push/print [""]
     :push/string []
     :push/grid []
     :push/ca-rule []
     ;; Level-1 (meta) substrate: holds the ARC task a solver reasons over.
     :push/task []
     [:push/vector :push/string] []
     [:push/vector :push/boolean] []
     [:push/vector :push/integer] []
     [:push/vector :push/float] []
     [:push/vector :push/char] []}}))

;; =============================================================
