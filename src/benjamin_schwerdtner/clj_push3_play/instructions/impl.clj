(ns benjamin-schwerdtner.clj-push3-play.instructions.impl
  (:require
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

(defn execute-instruction
  "Returns a new `state` with instruction `instr` executed.

  instr follows an instruction spec:

  in: A vector of input stack types, can be empty.

  out: An output stack type, or the keyword `:state` to signify
       state level transformation.

  f: A transform function, where the first arg is the state,
     successive args are `:in` arguments.

  "
  [state {:as instr :push.instruction/keys [in out f]}]
  (let [{:push.instruction/keys [in out f]} instr
        in-vals-info (reduce (fn [[s vs] t]
                               (let [stack
                                     (get-in s [:stacks t])]
                                 (if (empty? stack)
                                   (ensure-reduced ::empty)
                                   [(update-in s [:stacks t] pop)
                                    (conj vs (peek stack))])))
                             [state []]
                             in)]
    (if (= in-vals-info ::empty)
      ;; NOOP
      ;;
      state
      (let [[state in-vals] in-vals-info
            [outcome err?] (try
                             [(apply f state in-vals) false]
                             (catch Exception e
                               (if (:exceptions-are-warnings? state)
                                 (binding [*out* *err*] (println e))
                                 (throw e))
                               [nil true]))]
        (cond
          ;; Exceptions are NOOPS, still pop the args and make
          ;; program go forward.
          err? state
          (= out :state) outcome
          :else (stack/push state out outcome))))))

(def max-stack-items
  "Ceiling on how many items one instruction may add to a stack.

  Only the count-driven duplicators need it, and they need it badly: n comes
  off the INTEGER stack, which is clamped at 1e10, so `:integer_dup_times`
  with a large n on the stack would try to build a ten-billion-item vector."
  1000)

(defonce instruction-table (atom {}))

(defn register-instruction
  [{:keys [sym-name in out f]}]
  (swap! instruction-table assoc
         sym-name
         {:push.instruction/in in
          :push.instruction/out out
          :push.instruction/f f
          :push.instruction/sym-name sym-name}))

(defn instruction [sym]
  (get @instruction-table sym :no-instruction))

(defn all-instructions [] (keys @instruction-table))

(defn all-identifiers
  "What random code generation may draw from: instructions plus the NAMES of
  anything defined at runtime.

  Names, not bodies. This used to take `vals` of the bindings, which spliced a
  defined abstraction in as its own source every time it was drawn - inlined
  rather than referenced. That defeats the point of naming it: a library can
  only compress if a discovered abstraction costs one token at the point of
  use. `:code_instructions` alongside in instructions/code.clj already used
  `keys`; the two disagreed and this one was wrong."
  [state]
  (concat
   (:instructions state)
   (keys (:bindings state))))

(comment
  ;; ---------------------
  ;; user:

  (register-instruction
   {:sym-name :boolean_and
    :in [:push/boolean :push/boolean]
    :out :push/boolean
    :f (fn [_ a b] (and a b))})

  (when-let [inst (instruction :boolean_and)]
    (let [state {:stacks {:push/boolean [true true]}}]
      (execute-instruction state inst))))

;; ------------------------

(defn define-impl
  "Defines the identifier `push-name` as an instruction that
  puts the value onto the exec stack."
  [state push-name value]
  (assoc-in state [:bindings push-name] value))

;; -------------------------------

;; == operators ==
;; ----------------------------

(defn shover [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in [push-type :push/integer]
   :out :state
   :f (fn [state item index]
        (stack/shove-item state push-type item index))})

(defn yankdupper [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in [:push/integer]
   :out :state
   :f (fn [state index]
        (stack/yankdup-item state push-type index))})

(defn yanker [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in [:push/integer]
   :out :state
   :f (fn [state index]
        (stack/yank-item state push-type index))})

(defn flusher [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in []
   :out :state
   :f (fn [state]
        (stack/flush-stack state push-type))})

(defn rotater [{:keys [sym-name push-type]}]
  {:sym-name sym-name
   :in []
   :out :state
   :f (fn [state]
        (stack/rotate-top-three state push-type))})

(defn swapper
  [{:keys [sym-name push-type]}]
  {:f (fn [state] (stack/swap-top-two state push-type))
   :in []
   :out :state
   :sym-name sym-name})

(defn dupper
  [{:keys [sym-name push-type]}]
  {:f (fn [state] (stack/duplicate-top state push-type))
   :in []
   :out :state
   :sym-name sym-name})

(defn popper
  [{:keys [sym-name push-type]}]
  {:f (fn [s _] s)
   :in [push-type]
   :out :state
   :sym-name sym-name})

(defn stack-depth-impl
  [{:keys [sym-name push-type]}]
  {:f (fn [state]
        (stack/stack-depth state push-type))
   :in []
   :out :push/integer
   :sym-name sym-name})

(defn definer
  [{:keys [sym-name push-type]}]
  {:f define-impl
   :in [:push/name push-type]
   :out :state
   :sym-name sym-name})

;; ---------------------------------------------------------------------------
;; The polymorphic operators Push3 does not specify but PushGP relies on.
;;
;; `_empty` is the one that matters for evolvability: without it a program has
;; no way to ASK whether a stack has anything on it, so every consumer has to
;; be written to survive being starved. With it, a program can branch on the
;; shape of its own state, which is what makes loops over a vector terminate.
;; ---------------------------------------------------------------------------

(defn emptyer
  [{:keys [sym-name push-type]}]
  {:doc "Pushes true onto BOOLEAN if this stack is empty."
   :f (fn [state] (stack/empty-stack? state push-type))
   :in []
   :out :push/boolean
   :sym-name sym-name})

(defn- takes-index?
  "Whether an index/count instruction on `push-type` has its arguments.

  When the operand stack IS the INTEGER stack, the count and the item it
  applies to are the same stack, so two items are needed rather than one."
  [state push-type]
  (if (= push-type :push/integer)
    (<= 2 (stack/stack-depth state :push/integer))
    (and (not (stack/empty-stack? state :push/integer))
         (not (stack/empty-stack? state push-type)))))

(defn dup-timeser
  [{:keys [sym-name push-type]}]
  {:doc "Leaves n copies of the top item, n from INTEGER. n<=0 pops it."
   :f (fn [state]
        (if-not (takes-index? state push-type)
          state
          (let [n (stack/peek-item state :push/integer)
                state (stack/pop-item state :push/integer)
                item (stack/peek-item state push-type)]
            (if (pos? n)
              ;; The item is already there once, so n copies means n-1 more.
              ;; Deliberately not popping it: popping and re-pushing would
              ;; make n=1 a no-op the long way round, and would lose the item
              ;; for n<=0 anyway, which is what the pop branch is for.
              (reduce (fn [s _] (stack/push s push-type item))
                      state
                      (range (dec (min n max-stack-items))))
              (stack/pop-item state push-type)))))
   :in []
   :out :state
   :sym-name sym-name})

(defn dup-itemser
  [{:keys [sym-name push-type]}]
  {:doc "Duplicates the top n items, one time each, n from INTEGER."
   :f (fn [state]
        (if (stack/empty-stack? state :push/integer)
          state
          (let [n (stack/peek-item state :push/integer)
                state (stack/pop-item state :push/integer)
                stack (get-in state [:stacks push-type])
                n (min n (count stack) max-stack-items)]
            (if-not (pos? n)
              state
              ;; Top n items, bottom-of-those first, so the copies land in the
              ;; same order as the originals rather than reversed.
              (reduce (fn [s item] (stack/push s push-type item))
                      state
                      (subvec stack (- (count stack) n)))))))
   :in []
   :out :state
   :sym-name sym-name})

(defn deep-dupper
  [{:keys [sym-name push-type]}]
  {:doc "Pushes a copy of an item indexed from the BOTTOM of the stack."
   :f (fn [state]
        (if-not (takes-index? state push-type)
          state
          (let [index (stack/peek-item state :push/integer)
                state (stack/pop-item state :push/integer)
                stack (get-in state [:stacks push-type])
                ;; Bottom of the stack is index 0 - our stacks are vectors
                ;; with the top at the END, so this is a plain nth.
                index (max 0 (min index (dec (count stack))))]
            (stack/push state push-type (nth stack index)))))
   :in []
   :out :state
   :sym-name sym-name})

(def polymorphic-operators
  "The generators that apply to every stack type, as suffix -> generator.

  Registered across all types by instructions/polymorphic.clj. The per-type
  files register their own `dup`/`pop`/`swap`/... rather than going through
  here, which is why those are absent."
  {"_empty" emptyer
   "_dup_times" dup-timeser
   "_dup_items" dup-itemser
   "_deep_dup" deep-dupper})
