(ns benjamin-schwerdtner.clj-push3-play.instructions.exec
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.util :refer [ensure-list]]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]))

(register-instruction
 {:sym-name :exec_=
  :in [:push/exec :push/exec]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

;; =====================

;; Very similar to code.do*range, but operates on the EXEC stack.

(defn do-range
  "EXEC.DO*RANGE:

  An iteration instruction that executes the top item on the EXEC stack a number of times that depends on the top two integers,
  while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop.


  This is similar to CODE.DO*COUNT except that it takes its code argument from the EXEC stack.

  The top integer is the \"destination index\" and the second integer is the \"current index.\"

  Note that the range is inclusive of both endpoints.
  "

  [state code-to-do destination-index current-index]
  ;; First the code and the integer arguments are saved locally and popped.
  (let [increment (cond (< current-index destination-index) 1
                        (> current-index destination-index) -1
                        :else 0)
        continuation (if (zero? increment)
                       state
                       (stack/push
                        state
                        :push/exec
                        (list
                         ;; first a recursive call to EXEC.DO*RANGE
                         ;; with the same code and destination index,
                         ;; but with a current index that has been either incremented or decremented by 1 to be closer to the destination index
                         (+ current-index increment)
                         destination-index
                         :exec_do*range
                         ;; ------------------------------------
                         ;; and then the body code.
                         code-to-do)))]
    ;; If the integers are equal then the current index is pushed onto the INTEGER stack
    ;; and the code (which is the \"body\" of the loop) is pushed onto the EXEC stack
    (->
     continuation
     ;; push the current index
     (stack/push :push/integer current-index)
     ;; push the to-do
     (stack/push :push/exec code-to-do))))

;; do count

(defn do-count
  "
    EXEC.DO*COUNT: An iteration instruction that performs a loop (the body of which is taken from the EXEC stack) the number of times indicated by the INTEGER argument,
  pushing an index (which runs from zero to one less than the number of iterations) onto the INTEGER stack prior to each execution of the loop body.


  This is similar to CODE.DO*COUNT except that it takes its code argument from the EXEC stack.

  This should be implemented as a macro that expands into a call to EXEC.DO*RANGE.

  EXEC.DO*COUNT takes a single INTEGER argument (the number of times that the loop will be executed) and a single EXEC argument (the body of the loop).

  If the provided INTEGER argument is negative or zero then this becomes a NOOP.

  Otherwise it expands into:
  ( 0 <1 - IntegerArg> EXEC.DO*RANGE <ExecArg> )

  "
  [state integer code]
  (if (<= integer 0)
    state
    (-> state
        (stack/push
         :push/exec
         (list 0 (dec integer) :exec_do*range code)))))

;; note this has a different order from the code.do*count version

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
        (list 0 (dec integer) :exec_do*range code))))))

;; (conj (list 2) 'foo)
;; (conj '(foo) (list 2))

(register-instruction
 {:sym-name :exec_do*range
  :in [:push/exec :push/integer :push/integer]
  :out :state
  :f do-range
  :doc "CODE.DO*RANGE: An iteration instruction that executes the top item on the CODE stack a number of times that depends on the top two integers,
  while also pushing the loop counter onto the INTEGER stack for possible access during the execution of the body of the loop.

  The top integer is the \"destination index\" and the second integer is the \"current index.\""})

(register-instruction
 {:sym-name :exec_do*count
  :in [:push/integer :push/exec]
  :out :state
  :f do-count
  :doc "Execute the code at the top of exec n times.
Where n is the top item of the Integer stack.

This expands to a call to exec_do*range
"})

(register-instruction
 {:f do-times
  :in [:push/integer :push/exec]
  :out :state
  :sym-name :exec_do*times})

;; EXEC.IF: If the top item of the BOOLEAN stack is TRUE then this removes the second item on the EXEC stack, leaving the first item to be executed. If it is false then it removes the first item, leaving the second to be executed. This is similar to CODE.IF except that it operates on the EXEC stack. This acts as a NOOP unless there are at least two items on the EXEC stack and one item on the BOOLEAN stack.

(register-instruction
 {:sym-name :exec_if
  :in [:push/boolean :push/exec :push/exec]
  :out :push/exec
  :doc "If the top item of the BOOLEAN stack is TRUE this recursively executes the second item of the CODE stack;
otherwise it recursively executes the first item of the CODE stack."
  :f (fn [_ condition alternative consequent]
       (if condition
         consequent
         alternative))})

;; If the top BOOLEAN is TRUE, leaves the first item on the EXEC stack to be
;; executed. Otherwise, it removes it. Acts as a NOOP unless there is at least
;; one item on the EXEC stack and one item on the BOOLEAN stack

(register-instruction
 {:sym-name :exec_when
  :in [:push/boolean :push/exec]
  :out :state
  :doc ""
  :f (fn [state condition consequent]
       (if condition
         (stack/push state :push/exec consequent)
         state))})

;; Keeps executing the top instruction on the EXEC stack while the top item on
;; the BOOLEAN stack is true

(register-instruction
 {:sym-name :exec_while
  :in [:push/boolean :push/exec]
  :out :state
  :doc "EXEC.WHILE: Keeps executing the top instruction on the EXEC stack while the top item on the BOOLEAN stack is true."
  :f (fn [state condition to-do]
       (if condition
         (-> state
             (stack/push :push/exec :exec_while)
             (stack/push :push/exec to-do))
         ;; If the condition is false, just return the state unchanged.
         state))})

;; Keeps executing the top instruction on the EXEC stack while the top item on
;; the BOOLEAN stack is true. Differs from :exec_while in that it executes
;; the top instruction at least once

(register-instruction
 {:sym-name :exec_do_while
  :in [:push/boolean :push/exec]
  :out :state
  :doc "EXEC.DO-WHILE: Keeps executing the top instruction on the EXEC stack while the top item on the BOOLEAN stack is true. Differs from EXEC.WHILE in that it executes the top instruction at least once."
  :f (fn [state condition to-do]
       (-> state
           (stack/push :push/exec :exec_while)
           (stack/push :push/exec to-do)))})

;; EXEC.K: The Push implementation of the "K combinator". Removes the second item on the EXEC stack.

(register-instruction
 {:sym-name :exec_k
  :in [:push/exec :push/exec]
  :out :push/exec
  :f (fn [_ a b] a)
  :doc "EXEC.K: The Push implementation of the \"K combinator\". Removes the second item on the EXEC stack."})

;; EXEC.S: The Push implementation of the "S combinator". Pops 3 items from the EXEC stack, which we will call A, B, and C (with A being the first one popped). Then pushes a list containing B and C back onto the EXEC stack, followed by another instance of C, followed by another instance of A.

(register-instruction
 {:sym-name :exec_s
  :in [:push/exec :push/exec :push/exec]
  :out :state
  :f (fn [state a b c]

       ;; (S a b c)
       ;;
       ;; =>  (a c (b c))

       (->
        (stack/push state :push/exec (list b c))
        (stack/push :push/exec c)
        (stack/push :push/exec a)))

  :doc "EXEC.S: The Push implementation of the \"S combinator\". Pops 3 items from the EXEC stack, which we will call A, B, and C (with A being the first one popped). Then pushes a list containing B and C back onto the EXEC stack, followed by another instance of C, followed by another instance of A."})

;; EXEC.Y: The Push implementation of the "Y combinator". Inserts beneath the top item of the EXEC stack a new item of the form "( EXEC.Y <TopItem> )".

(register-instruction
 {:sym-name :exec_y
  :in [:push/exec]
  :out :state
  :f (fn [state top-item]
       (-> state
           (stack/push :push/exec (list :exec_y top-item))
           (stack/push :push/exec top-item)))
  :doc "EXEC.Y: The Push implementation of the \"Y combinator\". Inserts beneath the top item of the EXEC stack a new item of the form \"( EXEC.Y <TopItem> )\"."})

;; Stack manipulation instructions
(doseq [[sym-name impl]
        [[:exec_stackdepth impl/stack-depth-impl]
         [:exec_pop impl/popper]
         [:exec_dup impl/dupper]
         [:exec_flush impl/flusher]
         [:exec_shove impl/shover]
         [:exec_yank impl/yanker]
         [:exec_yankdup impl/yankdupper]
         [:exec_rot impl/rotater]
         [:exec_swap impl/swapper]
         [:exec_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/exec})))
