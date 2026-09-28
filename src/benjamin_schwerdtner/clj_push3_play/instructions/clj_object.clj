(ns benjamin-schwerdtner.clj-push3-play.instructions.clj-object
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [clojure.set]))

;; Type checking instructions

(register-instruction
 {:sym-name :clj-object_type
  :in [:push/clj-object]
  :out :push/string
  :f (fn [_ obj] (str (type obj)))})

(register-instruction
 {:sym-name :clj-object_nil?
  :in [:push/clj-object]
  :out :push/boolean
  :f (fn [_ obj] (nil? obj))})

(register-instruction
 {:sym-name :clj-object_coll?
  :in [:push/clj-object]
  :out :push/boolean
  :f (fn [_ obj] (coll? obj))})

(register-instruction
 {:sym-name :clj-object_map?
  :in [:push/clj-object]
  :out :push/boolean
  :f (fn [_ obj] (map? obj))})

(register-instruction
 {:sym-name :clj-object_vector?
  :in [:push/clj-object]
  :out :push/boolean
  :f (fn [_ obj] (vector? obj))})

(register-instruction
 {:sym-name :clj-object_seq?
  :in [:push/clj-object]
  :out :push/boolean
  :f (fn [_ obj] (seq? obj))})

(register-instruction
 {:sym-name :clj-object_empty?
  :in [:push/clj-object]
  :out :push/boolean
  :f (fn [_ obj] (empty? obj))})

;; Collection operations

(register-instruction
 {:sym-name :clj-object_count
  :in [:push/clj-object]
  :out :push/integer
  :f (fn [_ obj] (count obj))})

(register-instruction
 {:sym-name :clj-object_first
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ obj] (first obj))})

(register-instruction
 {:sym-name :clj-object_rest
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ obj] (rest obj))})

(register-instruction
 {:sym-name :clj-object_cons
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ elem coll] (cons elem coll))})

(register-instruction
 {:sym-name :clj-object_conj
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll elem] (conj coll elem))})

(register-instruction
 {:sym-name :clj-object_into
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ to from] (into to from))})

;; Map operations

(register-instruction
 {:sym-name :clj-object_get
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll key] (get coll key))})

(register-instruction
 {:sym-name :clj-object_get-in
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll keys] (get-in coll keys))})

(register-instruction
 {:sym-name :clj-object_assoc
  :in [:push/clj-object :push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll key val] (assoc coll key val))})

(register-instruction
 {:sym-name :clj-object_dissoc
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll key] (dissoc coll key))})

(register-instruction
 {:sym-name :clj-object_keys
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ obj] (keys obj))})

(register-instruction
 {:sym-name :clj-object_vals
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ obj] (vals obj))})

(register-instruction
 {:sym-name :clj-object_merge
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ m1 m2] (merge m1 m2))})

;; Sequence operations - These work with code on the code stack, not exec

(register-instruction
 {:sym-name :clj-object_take
  :in [:push/integer :push/clj-object]
  :out :push/clj-object
  :f (fn [_ n coll] (take n coll))})

(register-instruction
 {:sym-name :clj-object_drop
  :in [:push/integer :push/clj-object]
  :out :push/clj-object
  :f (fn [_ n coll] (drop n coll))})

(register-instruction
 {:sym-name :clj-object_reverse
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll] (reverse coll))})

(register-instruction
 {:sym-name :clj-object_sort
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll] (sort coll))})

(register-instruction
 {:sym-name :clj-object_partition
  :in [:push/integer :push/clj-object]
  :out :push/clj-object
  :f (fn [_ n coll] (partition n coll))})

(register-instruction
 {:sym-name :clj-object_flatten
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll] (flatten coll))})

(register-instruction
 {:sym-name :clj-object_distinct
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll] (distinct coll))})

(register-instruction
 {:sym-name :clj-object_frequencies
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll] (frequencies coll))})

(register-instruction
 {:sym-name :clj-object_group-by
  :in [:push/code :push/clj-object]
  :out :push/clj-object
  :f (fn [_ f coll]
       (group-by (fn [x] (eval (list f x))) coll))})

;; Set operations

(register-instruction
 {:sym-name :clj-object_union
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ s1 s2] (clojure.set/union (set s1) (set s2)))})

(register-instruction
 {:sym-name :clj-object_intersection
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ s1 s2] (clojure.set/intersection (set s1) (set s2)))})

(register-instruction
 {:sym-name :clj-object_difference
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ s1 s2] (clojure.set/difference (set s1) (set s2)))})

;; String/keyword operations

(register-instruction
 {:sym-name :clj-object_keyword
  :in [:push/string]
  :out :push/clj-object
  :f (fn [_ s] (keyword s))})

(register-instruction
 {:sym-name :clj-object_name
  :in [:push/clj-object]
  :out :push/string
  :f (fn [_ obj] (name obj))})

(register-instruction
 {:sym-name :clj-object_namespace
  :in [:push/clj-object]
  :out :push/string
  :f (fn [_ obj] (namespace obj))})

;; Vector operations

(register-instruction
 {:sym-name :clj-object_nth
  :in [:push/clj-object :push/integer]
  :out :push/clj-object
  :f (fn [_ coll idx] (nth coll idx nil))})

(register-instruction
 {:sym-name :clj-object_vec
  :in [:push/clj-object]
  :out :push/clj-object
  :f (fn [_ coll] (vec coll))})

;; Comparison

(register-instruction
 {:sym-name :clj-object_=
  :in [:push/clj-object :push/clj-object]
  :out :push/boolean
  :f (fn [_ a b] (= a b))})

;; Construction

(register-instruction
 {:sym-name :clj-object_list
  :in []
  :out :push/clj-object
  :f (fn [_] '())})

(register-instruction
 {:sym-name :clj-object_vector
  :in []
  :out :push/clj-object
  :f (fn [_] [])})

(register-instruction
 {:sym-name :clj-object_map
  :in []
  :out :push/clj-object
  :f (fn [_] {})})

(register-instruction
 {:sym-name :clj-object_set
  :in []
  :out :push/clj-object
  :f (fn [_] #{})})

(register-instruction
 {:sym-name :clj-object_range
  :in [:push/integer :push/integer]
  :out :push/clj-object
  :f (fn [_ start end] (range start end))})

;; Conversion from other types

(register-instruction
 {:sym-name :clj-object_from-string
  :in [:push/string]
  :out :push/clj-object
  :f (fn [_ s] (read-string s))})

(register-instruction
 {:sym-name :clj-object_to-string
  :in [:push/clj-object]
  :out :push/string
  :f (fn [_ obj] (pr-str obj))})

(register-instruction
 {:sym-name :clj-object_from-integer
  :in [:push/integer]
  :out :push/clj-object
  :f (fn [_ n] n)})

(register-instruction
 {:sym-name :clj-object_from-float
  :in [:push/float]
  :out :push/clj-object
  :f (fn [_ n] n)})

(register-instruction
 {:sym-name :clj-object_from-boolean
  :in [:push/boolean]
  :out :push/clj-object
  :f (fn [_ b] b)})

;; Update operations

(register-instruction
 {:sym-name :clj-object_update
  :in [:push/clj-object :push/clj-object :push/code]
  :out :push/clj-object
  :f (fn [_ coll key f]
       (update coll key (fn [v] (eval (list f v)))))})

(register-instruction
 {:sym-name :clj-object_update-in
  :in [:push/clj-object :push/clj-object :push/code]
  :out :push/clj-object
  :f (fn [_ coll keys f]
       (update-in coll keys (fn [v] (eval (list f v)))))})

;; Contains operations

(register-instruction
 {:sym-name :clj-object_contains?
  :in [:push/clj-object :push/clj-object]
  :out :push/boolean
  :f (fn [_ coll key] (contains? coll key))})

(register-instruction
 {:sym-name :clj-object_some?
  :in [:push/clj-object]
  :out :push/boolean
  :f (fn [_ obj] (some? obj))})

;; Threading operations

(register-instruction
 {:sym-name :clj-object_juxt
  :in [:push/clj-object :push/clj-object]
  :out :push/clj-object
  :f (fn [_ obj fns]
       (mapv #(eval (list % obj)) fns))})

;; Stack manipulation instructions

(doseq [[sym-name impl]
        [[:clj-object_stackdepth impl/stack-depth-impl]
         [:clj-object_pop impl/popper]
         [:clj-object_dup impl/dupper]
         [:clj-object_flush impl/flusher]
         [:clj-object_shove impl/shover]
         [:clj-object_yank impl/yanker]
         [:clj-object_yankdup impl/yankdupper]
         [:clj-object_rot impl/rotater]
         [:clj-object_swap impl/swapper]
         [:clj-object_define impl/definer]]]
  (register-instruction
   (impl {:sym-name sym-name
          :push-type :push/clj-object})))