(ns benjamin-schwerdtner.clj-push3-play.instructions.code
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.util :as util :refer [ensure-list]]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   ;; [benjamin-schwerdtner.clj-push3-play.instructions.code-impl :refer
   ;;  [do-range do-count do-times
   ;;   ensure-list]]
   ))
;; CODE:

;; - usually a clojure list
;; - it is possible to push non-lists, then it counts as 'atom'.

;; Comparison instruction
(register-instruction
 {:sym-name :code_=
  :in [:push/code :push/code]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

;; Random code generation

;; CODE.RAND: Pushes a newly-generated random program onto the CODE stack. The limit for the size of the expression is taken from the INTEGER stack; to ensure that it is in the appropriate range this is taken modulo the value of the MAX-POINTS-IN-RANDOM-EXPRESSIONS parameter and the absolute value of the result is used.

(register-instruction
 {:f (fn [state limit]
       (gen/generate-code
        {:atom-generators (concat gen/default-generators
                                  (gen/identifier-generators
                                   (impl/all-identifiers state)))
         :max-points (abs (mod limit
                               (->
                                state
                                :parameters
                                (:max-points-in-random-expressions
                                 20))))}))
  :in [:push/integer]
  :out :push/code
  :sym-name :code_rand})

(register-instruction
 {:f (fn [state]
       (gen/rand-atom (concat gen/default-generators
                              (gen/identifier-generators
                               (impl/all-identifiers state)))))
  :in []
  :out :push/code
  :sym-name :code_rand_atom})

;; List manipulation instructions
(register-instruction
 {:sym-name :code_append
  :in [:push/code :push/code]
  :out :push/code
  :f (fn [_ a b]
       (apply list
              (concat (ensure-list a)
                      (ensure-list b))))})

(register-instruction
 {:sym-name :code_atom
  :in [:push/code]
  :out :push/boolean
  :f (fn [_ code]
       (not (list? code)))})

(register-instruction
 {:sym-name :code_car
  :in [:push/code]
  :out :stack
  :doc "CODE.CAR: Pushes the first item of the list on top of the CODE stack. For example, if the top piece of code is \"( A B )\" then this pushes \"A\" (after popping the argument). If the code on top of the stack is not a list then this has no effect. The name derives from the similar Lisp function; a more generic name would be \"FIRST\""
  :f (fn [state code]
       (if (list? code)
         (stack/push state :push/code (or (first code) '()))
         state))})

(register-instruction
 {:sym-name :code_cdr
  :in [:push/code]
  :out :state
  :doc "CODE.CDR: Pushes a version of the list from the top of the CODE stack without its first element. For example, if the top piece of code is \"( A B )\" then this pushes \"( B )\" (after popping the argument). If the code on top of the stack is not a list then this pushes the empty list (\"( )\"). The name derives from the similar Lisp function; a more generic name would be \"REST\"."
  :f (fn [state code]
       (if (list? code)
         (stack/push state :push/code (or (rest code) '()))
         state))})

(register-instruction
 {:sym-name :code_cons
  :in [:push/code :push/code]
  :out :push/code
  :doc "For example, if the top piece of code is \"( A B )\" and the second piece of code is \"X\" then this pushes \"( X A B )\" (after popping the argument)."
  :f (fn [_ cdr-list car-item]
       (into (ensure-list cdr-list) (list car-item)))})

;; CODE.CONTAINER: Pushes the "container" of the second CODE stack item within the first CODE stack item onto the CODE stack. If second item contains the first anywhere (i.e. in any nested list) then the container is the smallest sub-list that contains but is not equal to the first instance. For example, if the top piece of code is "( B ( C ( A ) ) ( D ( A ) ) )" and the second piece of code is "( A )" then this pushes ( C ( A ) ). Pushes an empty list if there is no such container.

;; This is like lispy raise sexp:

;; (a (c (a)) (d (a)))

;; (c (a))

(register-instruction
 {:sym-name :code_container
  :in [:push/code :push/code]
  :out :push/code
  :f (fn [_ container item]
       (util/containing-subtree container item))})

(comment
  (let [container '(a (c (a)) (d (a)))
        item '(a)]
    (util/containing-subtree container item))
  ;; (c (a))
  )
(register-instruction
 {:sym-name :code_contains
  :in [:push/code :push/code]
  :out :push/boolean
  :doc "Pushes true, if the second code stack item contains the first code stack item anywhere, is a sub-list."
  :f (fn [_ item container]
       (util/contains-subtree? container item))})

;; -----------------
;;
;; CODE.DO: Recursively invokes the interpreter on the program on top of the CODE stack. After evaluation the CODE stack is popped; normally this pops the program that was just executed, but if the expression itself manipulates the stack then this final pop may end up popping something else.
;;
;; Benni:
;; - I thought the point of EXEC was that we don't need host recursion.
;; - keeping track of when the 'code item' is done executing is intractable, since exec stack can be manipulated by the program itself.
;;
;;
;; - I reasoning thusly: If I want to manipulate the CODE stack item during the recursive execution, I am usually popping from the code stack, messing up the 'cleanup' phase of code_do anyway.
;; - The difference between this and duplicating the code before code_do* seems not to varrant changing the execution model
;;
;;
;; - in Clojush, is implement like this:

#_(define-registered
    code_do
    ^{:stack-types [:code :exec]}
    (fn [state]
      (if (not (empty? (:code state)))
        (push-item (stack-ref :code 0 state)
                   :exec
                   (push-item :code_pop :exec state))
        state)))

;; 1. push 'code pop' on exec stack
;; 2. push CODE on EXEC stack

;; then usually, code will be executed and the next instruction is a code pop.
;; Implementing the spec in spirit, but not by the letter. (because exec stack can be manipulated by the program, the pop instruction could be removed).
;;

(register-instruction
 {:sym-name :code_do
  :in [:push/code]
  :out :state
  :f (fn [state code]
       (-> state
           (stack/push :push/exec :code_pop)
           (stack/push :push/exec code)
           ;; also put it back because I just popped it from the args
           (stack/push :push/code code)))})

(register-instruction
 {:f (fn [_ code] code)
  :in [:push/code]
  :out :push/exec
  :sym-name :code_do*})

;; -------------------
;; =============================================

;; Reference implementation from Clojush
;; clojush/code.clj

;; do range

(defn do-range
  "CODE.DO*RANGE: An iteration instruction that executes the top item on the CODE stack a number of times that depends on the top two integers,
  while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop.


  The top integer is the \"destination index\" and the second integer is the \"current index.\"
  First the code and the integer arguments are saved locally and popped. Then the integers are compared.

  If the integers are equal then the current index is pushed onto the INTEGER stack and the code (which is the \"body\" of the loop) is pushed onto the EXEC stack for subsequent execution.

  If the integers are not equal then the current index will still be pushed onto the INTEGER stack but two items will be pushed onto the EXEC stack -- first a recursive call to CODE.DO*RANGE (with the same code and destination index, but with a current index that has been either incremented or decremented by 1 to be closer to the destination index) and then the body code. Note that the range is inclusive of both endpoints; a call with integer arguments 3 and 5 will cause its body to be executed 3 times, with the loop counter having the values 3, 4, and 5. Note also that one can specify a loop that \"counts down\" by providing a destination index that is less than the specified current index.


  Executes the code to do at leas once.

  "
  [state code-to-do destination-index current-index]
  (let [increment (cond (< current-index destination-index) 1
                        (> current-index destination-index) -1
                        :else 0)
        continuation (if (zero? increment)
                       state
                       (stack/push
                        state
                        :push/exec
                        (list (+ current-index increment)
                              destination-index
                              :code_quote
                              code-to-do
                              :code_do*range)))]

    (->
     continuation
     ;; push the current index
     (stack/push :push/integer current-index)
     ;; push the to-do
     (stack/push :push/exec code-to-do))))

;; do count

(defn do-count
  "CODE.DO*COUNT: An iteration instruction that performs a loop (the body of which is taken from the CODE stack)

  the number of times indicated by the INTEGER argument,

  Pushing an index (which runs from zero to one less than the number of iterations) onto the INTEGER stack prior to each execution of the loop body.

  This should be implemented as a macro that expands into a call to CODE.DO*RANGE.
  CODE.DO*COUNT takes a single INTEGER argument (the number of times that the loop will be executed) and a single CODE argument (the body of the loop).


  If the provided INTEGER argument is negative or zero then this becomes a NOOP. Otherwise it expands into:

  ( 0 <1 - IntegerArg> CODE.QUOTE <CodeArg> CODE.DO*RANGE )

"
  [state integer code]
  (if (<= integer 0)
    state
    (-> state
        (stack/push
         :push/exec
         (list 0 (dec integer) :code_quote code :code_do*range)))))

(defn do-times
  "Same as [[do-count]],

  but add a integer-pop to the front of the to-do code,
  so that the index argument is not tracked.
  "
  [state integer code]
  (if (<= integer 0)
    state
    (let [code
          (conj (ensure-list code) :integer_pop)]
      (->
       state
       (stack/push
        :push/exec
        (list 0 (dec integer) :code_quote code :code_do*range))))))

(register-instruction
 {:sym-name :code_do*range
  :in [:push/code :push/integer :push/integer]
  :out :state
  :f do-range
  :doc "CODE.DO*RANGE: An iteration instruction that executes the top item on the CODE stack a number of times that depends on the top two integers,
  while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop.

  The top integer is the \"destination index\" and the second integer is the \"current index.\""})

(register-instruction
 {:sym-name :code_do*count
  :in [:push/integer :push/code]
  :out :state
  :f do-count
  :doc "[[do-count]]"})

(register-instruction
 {:f do-times
  :in [:push/integer :push/code]
  :out :state
  :sym-name :code_do*times})

;; ------------------

(register-instruction
 {:sym-name :code_extract
  :in [:push/code :push/integer]
  :out :push/code
  :doc "Pushes the sub-expression of the top CODE item indexed by the top INTEGER item onto the CODE stack.

The indexing counts 'points', where each paranthezied expressin adn each literal/instruction is considered a point,
and it proceeds in depth first order.

The entire piece of code is at index 0; if is al list then the first item in the list
is at index 1, etc.

The integer used as the index is taken module the number of points in the overall expression
(and its absolute value is taken in case it is negative) to ensure that it is within the meaningful range.

"
  :f (fn [_ code index]
       (util/code-at-point code index))})

(register-instruction
 {:sym-name :code_insert
  :in [:push/code :push/code :push/integer]
  :out :push/code
  :doc "Pushes the result of inserting the second item of the code stack into the first item,
at the position indexed by the top integer.

Replaces whatever was there.

The indexing is like code_extract."

  :f (fn [_ container new-item index]
       (util/insert-code-at-point container new-item index))})

#_(define-registered
    code_if
    ^{:stack-types [:code :exec :boolean]}
    (fn [state]
      (if (not (or (empty? (:boolean state))
                   (empty? (rest (:code state)))))
        (push-item (if (first (:boolean state))
                     (first (rest (:code state)))
                     (first (:code state)))
                   :exec
                   (pop-item :boolean (pop-item :code (pop-item :code state))))
        state)))

(register-instruction
 {:sym-name :code_if
  :in [:push/boolean :push/code :push/code]
  :out :push/exec
  :doc "If the top item of the BOOLEAN stack is TRUE this recursively executes the second item of the CODE stack;
otherwise it recursively executes the first item of the CODE stack."
  :f (fn [_ condition alternative consequent]
       (if condition
         consequent
         alternative))})

(register-instruction
 {:sym-name :code_length
  :in [:push/code]
  :out :push/integer
  :f (fn [_ code]
       (if (list? code)
         (count code)
         1))})

(register-instruction
 {:sym-name :code_list
  :in [:push/code :push/code]
  :out :push/code
  :f (fn [_ a b]
       (list a b))})

(register-instruction
 {:sym-name :code_member
  :in [:push/code :push/code]
  :out :push/boolean
  :f (fn [_ container item]
       ;;
       ;; I wonder if I should allow code items to be seqs.
       ;; ie. make code_member work on the seq interface
       ;; It goes against Clojure sensibilities to define typed operators
       (if
        (or
         (map? container)
         (set? container))
         (contains? container item)
         (some #{item} (ensure-list container))))})

(register-instruction
 {:sym-name :code_noop
  :in []
  :out :state
  :f identity})

(register-instruction
 {:sym-name :code_nth
  :in [:push/integer :push/code]
  :out :push/code
  :f (fn [_ n code]
       (let [code (ensure-list code)]
         (if (empty? code)
           '()
           (let [n (mod (abs n) (count code))]
             (nth code n)))))})

(register-instruction
 {:sym-name :code_nthcdr
  :in [:push/integer :push/code]
  :out :push/code
  :f (fn [_ n code]
       (let [code (ensure-list code)]
         (if
          (empty? code)
           '()
           (let [n (mod (abs n) (count code))]
             (drop n code)))))})

(register-instruction
 {:sym-name :code_null
  :in [:push/code]
  :out :push/boolean
  :f (fn [_ code]
       (= '() code))})

(register-instruction
 {:sym-name :code_position
  :in [:push/code :push/code]
  :out :push/integer
  :f (fn [_ container item]
       (let [container (ensure-list container)
             item (ensure-list item)]
         (.indexOf container item)))})

(register-instruction
 {:sym-name :code_quote
  :in [:push/exec]
  :out :push/code
  :doc "CODE.QUOTE causes the next encountered piece of code, whatever it is, to be pushed onto the CODE stack rather than being executed.

NOTE: this moves the next item in the EXEC stack to the CODE stack.
"
  :f (fn [_ exec-item] exec-item)})

(register-instruction
 {:sym-name :code_size
  :in [:push/code]
  :out :push/integer
  :f (fn [_ code]
       (util/count-points code))})

(register-instruction
 {:sym-name :code_subst
  :in [:push/code :push/code :push/code]
  :out :push/code
  :doc "Pushes the result of substituting the third item on the code stack for the second item in the first item.
"
  :f (fn [_ container to-substitute to-be-substituted]
       (util/postwalk-replace-in-list {to-substitute to-be-substituted} container))})

;; ---------------------------------------

(register-instruction {:f (fn [_ bool] (boolean bool))
                       :in [:push/boolean]
                       :out :push/code
                       :sym-name :code_fromboolean})

(register-instruction {:f (fn [_ flt] flt)
                       :in [:push/float]
                       :out :push/code
                       :sym-name :code_fromfloat})

(register-instruction {:f (fn [_ int] int)
                       :in [:push/integer]
                       :out :push/code
                       :sym-name :code_frominteger})

(register-instruction {:f (fn [_ name] name)
                       :in [:push/name]
                       :out :push/code
                       :sym-name :code_fromname})

;; -----------------------------------

;; CODE.INSTRUCTIONS: Pushes a list of all active instructions in the interpreter's current configuration.

;; Is this with bound names or without?

(register-instruction
 {:f (fn [state] (:instructions state))
  :in []
  :out :push/code
  :sym-name :code_instructions*})

(register-instruction
 {:f (fn [state]
       (apply list
              (concat (:instructions state)
                      (keys (:bindings state)))))
  :in []
  :out :push/code
  :sym-name :code_instructions})

;; Stack manipulation instructions
(doseq [[sym-name impl]
        [[:code_stackdepth impl/stack-depth-impl]
         [:code_pop impl/popper]
         [:code_dup impl/dupper]
         [:code_flush impl/flusher]
         [:code_shove impl/shover]
         [:code_yank impl/yanker]
         [:code_yankdup impl/yankdupper]
         [:code_rot impl/rotater]
         [:code_swap impl/swapper]
         [:code_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/code})))
