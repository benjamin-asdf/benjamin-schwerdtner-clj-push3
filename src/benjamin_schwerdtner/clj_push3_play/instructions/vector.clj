(ns benjamin-schwerdtner.clj-push3-play.instructions.vector
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.generator :as gen]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [benjamin-schwerdtner.clj-push3-play.prot :as prot]))

;; Helper function to get the literal type for a vector type
(defn get-vector-literal-type [vector-type]
  (if (and (vector? vector-type)
           (= (first vector-type) :push/vector))
    (second vector-type)
    :push/integer))

;; Register instructions for each vector type
(doseq [elem-type [:push/boolean :push/integer :push/float :push/string :push/char]]
  (let [vector-type [:push/vector elem-type]
        type-suffix (name elem-type)]

    ;; BUTLAST
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_butlast"))
      :in [vector-type]
      :out vector-type
      :f (fn [_ v] (vec (butlast v)))})

    ;; CONCAT
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_concat"))
      :in [vector-type vector-type]
      :out vector-type
      :f (fn [_ v1 v2] (vec (concat v2 v1)))})

    ;; CONJ
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_conj"))
      :in [elem-type vector-type]
      :out vector-type
      :f (fn [_ elem v] (conj v elem))})

    ;; CONTAINS
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_contains"))
      :in [elem-type vector-type]
      :out :push/boolean
      :f (fn [_ elem v] (contains? (set v) elem))})

    ;; EMPTY?
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_empty?"))
      :in [vector-type]
      :out :push/boolean
      :f (fn [_ v] (empty? v))})

    ;; = ; every other stack type has its own equality (`:integer_=`,
    ;; `:string_=`, ...) and the vector stacks did not, which left a program
    ;; unable to ask whether two vectors it built are the same - the one
    ;; comparison a vector problem most obviously needs.
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_="))
      :in [vector-type vector-type]
      :out :push/boolean
      :f (fn [_ v1 v2] (= v1 v2))})

    ;; FIRST
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_first"))
      :in [vector-type]
      :out elem-type
      :f (fn [_ v] (if (empty? v)
                     (case elem-type
                       :push/integer 0
                       :push/float 0.0
                       :push/string ""
                       :push/boolean false
                       :push/char \space)
                     (first v)))})

    ;; INDEXOF
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_indexof"))
      :in [elem-type vector-type]
      :out :push/integer
      :f (fn [_ elem v]
           (let [idx (.indexOf v elem)]
             (if (= idx -1) -1 idx)))})

    ;; LAST
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_last"))
      :in [vector-type]
      :out elem-type
      :f (fn [_ v] (if (empty? v)
                     (case elem-type
                       :push/integer 0
                       :push/float 0.0
                       :push/string ""
                       :push/boolean false
                       :push/char \space)
                     (last v)))})

    ;; LENGTH
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_length"))
      :in [vector-type]
      :out :push/integer
      :f (fn [_ v] (count v))})

    ;; NTH
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_nth"))
      :in [:push/integer vector-type]
      :out elem-type
      :f (fn [_ n v]
           (if (empty? v)
             (case elem-type
               :push/integer 0
               :push/float 0.0
               :push/string ""
               :push/boolean false
               :push/char \space)
             (nth v (mod n (count v)))))})

    ;; OCCURRENCESOF
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_occurrencesof"))
      :in [elem-type vector-type]
      :out :push/integer
      :f (fn [_ elem v] (count (filter #(= elem %) v)))})

    ;; PUSHALL
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_pushall"))
      :in [vector-type]
      :out :state
      :f (fn [state v]
           (reduce (fn [st elem] (stack/push st elem-type elem))
                   state
                   (reverse v)))})

    ;; REMOVE
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_remove"))
      :in [elem-type vector-type]
      :out vector-type
      :f (fn [_ elem v] (vec (filter #(not= elem %) v)))})

    ;; REPLACE
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_replace"))
      :in [elem-type elem-type vector-type]
      :out vector-type
      :f (fn [_ new-elem old-elem v]
           (vec (replace {old-elem new-elem} v)))})

    ;; REPLACEFIRST
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_replacefirst"))
      :in [elem-type elem-type vector-type]
      :out vector-type
      :f (fn [_ new-elem old-elem v]
           (let [idx (.indexOf v old-elem)]
             (if (= idx -1)
               v
               (assoc v idx new-elem))))})

    ;; REST
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_rest"))
      :in [vector-type]
      :out vector-type
      :f (fn [_ v] (vec (rest v)))})

    ;; REVERSE
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_reverse"))
      :in [vector-type]
      :out vector-type
      :f (fn [_ v] (vec (reverse v)))})

    ;; SET
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_set"))
      :in [elem-type :push/integer vector-type]
      :out vector-type
      :f (fn [_ elem n v]
           (if (empty? v)
             v
             (assoc v (mod n (count v)) elem)))})

    ;; SUBVEC
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_subvec"))
      :in [:push/integer :push/integer vector-type]
      :out vector-type
      :f (fn [_ start-raw stop-raw v]
           (let [start (min (count v) (max 0 start-raw))
                 stop (min (count v) (max start stop-raw))]
             (subvec v start stop)))})

    ;; TAKE
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_take"))
      :in [:push/integer vector-type]
      :out vector-type
      :f (fn [_ n v] (vec (take n v)))})

    ;; EMPTY VECTOR
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_emptyvector"))
      :in []
      :out vector-type
      :f (fn [_] [])})

    ;; FROM LIST (convert code/list to vector)
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_fromlist"))
      :in [:push/code]
      :out vector-type
      :f (fn [_ lst]
           (vec (filter #(= (prot/m-typeof-item %) elem-type) lst)))})

    ;; ITERATE
    (register-instruction
     {:sym-name (keyword (str "vector_" type-suffix "_iterate"))
      :in [vector-type :push/exec]
      :out :state
      :f (fn [state v exec-item]
           (if (empty? v)
             state
             (cond
               (empty? (rest v))
               (-> state
                   (stack/push elem-type (first v)))

               :else
               (-> state
                   (stack/push :push/exec (keyword (str "vector_" type-suffix "_iterate")))
                   (stack/push :push/exec (vec (rest v)))
                   (stack/push :push/exec exec-item)
                   (stack/push elem-type (first v))))))})

    ;; Stack manipulation
    (run! register-instruction
          [(impl/dupper {:sym-name (keyword (str "vector_" type-suffix "_dup"))
                         :push-type vector-type})
           (impl/popper {:sym-name (keyword (str "vector_" type-suffix "_pop"))
                         :push-type vector-type})
           (impl/swapper {:sym-name (keyword (str "vector_" type-suffix "_swap"))
                          :push-type vector-type})
           (impl/rotater {:sym-name (keyword (str "vector_" type-suffix "_rot"))
                          :push-type vector-type})
           (impl/flusher {:sym-name (keyword (str "vector_" type-suffix "_flush"))
                          :push-type vector-type})
           (impl/yanker {:sym-name (keyword (str "vector_" type-suffix "_yank"))
                         :push-type vector-type})
           (impl/yankdupper {:sym-name (keyword (str "vector_" type-suffix "_yankdup"))
                             :push-type vector-type})
           (impl/shover {:sym-name (keyword (str "vector_" type-suffix "_shove"))
                         :push-type vector-type})
           (impl/stack-depth-impl {:sym-name (keyword (str "vector_" type-suffix "_stackdepth"))
                                   :push-type vector-type})])))
