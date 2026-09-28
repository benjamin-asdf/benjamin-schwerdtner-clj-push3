(ns benjamin-schwerdtner.clj-push3-play.instructions.push-name
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [random-seed.core :refer [rand-nth]]))

;; ==================================
;; NAME instructions

;; NAME.= instruction
(register-instruction
 {:sym-name :name_=
  :in [:push/name :push/name]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

;; Stack manipulation instructions using impl generators
(doseq [[sym-name impl-fn]
        [[:name_stackdepth impl/stack-depth-impl]
         [:name_pop impl/popper]
         [:name_dup impl/dupper]
         [:name_flush impl/flusher]
         [:name_shove impl/shover]
         [:name_yank impl/yanker]
         [:name_yankdup impl/yankdupper]
         [:name_rot impl/rotater]
         [:name_swap impl/swapper]]]
  (register-instruction
   (impl-fn {:sym-name sym-name
             :push-type :push/name})))

;; NAME.QUOTE instruction
;; is handled in benjamin-schwerdtner.clj-push3-play.interpreter

;; NAME.RAND instruction
(register-instruction
 {:sym-name :name_rand
  :in []
  :out :push/name
  :f (fn [_]
       (gensym "push_gensym_rand_"))})

;; NAME.RANDBOUNDNAME instruction
(register-instruction
 {:sym-name :name_randboundname
  :in []
  :out :state
  :f (fn [state]
       (let [bound-names (keys (:bindings state))]
         (if (empty? bound-names)
           state
           (stack/push state :push/name
                       (rand-nth (vec bound-names))))))})
