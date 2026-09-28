(ns benjamin-schwerdtner.clj-push3-play.instructions.string
  "String instructions for Push3.

  Adapted from propeller's string instructions to work with clj-push3-play.
  These instructions enable program synthesis for string-output problems
  from the PSB1/PSB2 benchmark suites.

  Source reference: propeller_problems/push/instructions/string.cljc

  Instruction naming convention:
  - string_X: Operations on strings (e.g., string_concat, string_length)
  - string_from_X: Convert other types to string (e.g., string_from_integer)

  Key differences from propeller:
  - Uses keywords for instruction names (e.g., :string_concat)
  - Stack type is :push/string instead of :string
  - State structure uses {:stacks {:push/string [...]}} format"
  (:require
   [benjamin-schwerdtner.clj-push3-play.instructions.impl :refer
    [register-instruction] :as impl]
   [benjamin-schwerdtner.clj-push3-play.stack.pushstate :as stack]
   [clojure.string :as str]
   [random-seed.core :as rand])
  (:refer-clojure :exclude [rand rand-int rand-nth]))

;; =============================================================================
;; Basic String Operations
;; =============================================================================

(register-instruction
 {:sym-name :string_length
  :in [:push/string]
  :out :push/integer
  :f (fn [_ s] (count s))})

(register-instruction
 {:sym-name :string_concat
  :in [:push/string :push/string]
  :out :push/string
  :f (fn [_ s1 s2] (str s2 s1))})

(register-instruction
 {:sym-name :string_reverse
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (apply str (reverse s)))})

(register-instruction
 {:sym-name :string_=
  :in [:push/string :push/string]
  :out :push/boolean
  :f (fn [_ s1 s2] (= s1 s2))})

;; =============================================================================
;; String Creation / Conversion
;; =============================================================================

(register-instruction
 {:sym-name :string_from_integer
  :in [:push/integer]
  :out :push/string
  :f (fn [_ i] (str i))})

(register-instruction
 {:sym-name :string_from_float
  :in [:push/float]
  :out :push/string
  :f (fn [_ f] (str f))})

(register-instruction
 {:sym-name :string_from_boolean
  :in [:push/boolean]
  :out :push/string
  :f (fn [_ b] (str b))})

(register-instruction
 {:sym-name :string_from_char
  :in [:push/char]
  :out :push/string
  :f (fn [_ c] (str c))})

;; =============================================================================
;; Character Access
;; =============================================================================

(register-instruction
 {:sym-name :string_first
  :in [:push/string]
  :out :push/char
  :f (fn [_ s]
       (when (seq s)
         (first s)))})

(register-instruction
 {:sym-name :string_last
  :in [:push/string]
  :out :push/char
  :f (fn [_ s]
       (when (seq s)
         (last s)))})

(register-instruction
 {:sym-name :string_nth
  :in [:push/string :push/integer]
  :out :push/char
  :f (fn [_ s i]
       (when (and (seq s) (>= i 0))
         (nth s (mod i (count s)))))})

(register-instruction
 {:sym-name :string_rest
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (apply str (rest s)))})

(register-instruction
 {:sym-name :string_butlast
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (apply str (butlast s)))})

;; =============================================================================
;; Substring Operations
;; =============================================================================

(register-instruction
 {:sym-name :string_take
  :in [:push/string :push/integer]
  :out :push/string
  :f (fn [_ s n]
       (apply str (take (max 0 n) s)))})

(register-instruction
 {:sym-name :string_drop
  :in [:push/string :push/integer]
  :out :push/string
  :f (fn [_ s n]
       (apply str (drop (max 0 n) s)))})

(register-instruction
 {:sym-name :string_substr
  :in [:push/string :push/integer :push/integer]
  :out :push/string
  :f (fn [_ s start end]
       (let [len (count s)
             start (min len (max 0 start))
             end (min len (max start end))]
         (subs s start end)))})

;; =============================================================================
;; String Predicates
;; =============================================================================

(register-instruction
 {:sym-name :string_empty?
  :in [:push/string]
  :out :push/boolean
  :f (fn [_ s] (empty? s))})

(register-instruction
 {:sym-name :string_contains
  :in [:push/string :push/string]
  :out :push/boolean
  :f (fn [_ s1 s2] (str/includes? s1 s2))})

(register-instruction
 {:sym-name :string_contains_char
  :in [:push/string :push/char]
  :out :push/boolean
  :f (fn [_ s c] (str/includes? s (str c)))})

;; =============================================================================
;; String Modification
;; =============================================================================

(register-instruction
 {:sym-name :string_replace
  :in [:push/string :push/string :push/string]
  :out :push/string
  :f (fn [_ s match replacement]
       (str/replace s match replacement))})

(register-instruction
 {:sym-name :string_replace_first
  :in [:push/string :push/string :push/string]
  :out :push/string
  :f (fn [_ s match replacement]
       (str/replace-first s match replacement))})

(register-instruction
 {:sym-name :string_replace_char
  :in [:push/string :push/char :push/char]
  :out :push/string
  :f (fn [_ s old-char new-char]
       (str/replace s old-char new-char))})

(register-instruction
 {:sym-name :string_replace_first_char
  :in [:push/string :push/char :push/char]
  :out :push/string
  :f (fn [_ s old-char new-char]
       (str/replace-first s old-char new-char))})

(register-instruction
 {:sym-name :string_remove_char
  :in [:push/string :push/char]
  :out :push/string
  :f (fn [_ s c]
       (apply str (filter #(not= c %) s)))})

;; The empty string, as a nullary instruction rather than only as a literal a
;; problem's ERC list has to remember to include. Several PSB problems have ""
;; as their answer for some inputs, and a program that has no "" anywhere in
;; it cannot reach that answer by any amount of mutation.
(register-instruction
 {:sym-name :string_empty_string
  :in []
  :out :push/string
  :f (fn [_] "")})

;; =============================================================================
;; Case Operations
;; =============================================================================

(register-instruction
 {:sym-name :string_uppercase
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (str/upper-case s))})

(register-instruction
 {:sym-name :string_lowercase
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (str/lower-case s))})

(register-instruction
 {:sym-name :string_capitalize
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (str/capitalize s))})

;; =============================================================================
;; Character/String Append
;; =============================================================================

(register-instruction
 {:sym-name :string_conj_char
  :in [:push/string :push/char]
  :out :push/string
  :f (fn [_ s c] (str s c))})

;; =============================================================================
;; Splitting and Parsing
;; =============================================================================

(register-instruction
 {:sym-name :string_split
  :in [:push/string]
  :out :state
  :f (fn [state s]
       (let [parts (str/split (str/trim s) #"\s+")]
         (reduce (fn [st part] (stack/push st :push/string part))
                 state
                 (reverse parts))))})

(register-instruction
 {:sym-name :string_parse_to_chars
  :in [:push/string]
  :out :state
  :f (fn [state s]
       (reduce (fn [st c] (stack/push st :push/char c))
               state
               (reverse s)))})

;; =============================================================================
;; Index Operations
;; =============================================================================

(register-instruction
 {:sym-name :string_indexof_char
  :in [:push/string :push/char]
  :out :push/integer
  :f (fn [_ s c]
       (let [idx (str/index-of s (str c))]
         (if idx idx -1)))})

(register-instruction
 {:sym-name :string_occurences_char
  :in [:push/string :push/char]
  :out :push/integer
  :f (fn [_ s c]
       (count (filter #(= c %) s)))})

;; =============================================================================
;; Set Character
;; =============================================================================

(register-instruction
 {:sym-name :string_set_char
  :in [:push/string :push/integer :push/char]
  :out :push/string
  :f (fn [_ s i c]
       (if (empty? s)
         s
         (let [idx (mod i (count s))]
           (str (subs s 0 idx) c (subs s (inc idx))))))})

;; =============================================================================
;; Iteration (uses exec stack)
;; =============================================================================

(register-instruction
 {:sym-name :string_iterate
  :in [:push/string]
  :out :state
  :f (fn [state s]
       ;; Check if there's something on exec stack to iterate with
       (if (empty? (get-in state [:stacks :push/exec]))
         state
         (if (empty? s)
           ;; Empty string - pop exec
           (update-in state [:stacks :push/exec] pop)
           (let [exec-top (peek (get-in state [:stacks :push/exec]))]
             (if (= 1 (count s))
               ;; Single char - just push the char
               (stack/push state :push/char (first s))
               ;; Multiple chars - push iteration setup
               (-> state
                   (stack/push :push/exec :string_iterate)
                   (stack/push :push/exec (apply str (rest s)))
                   (stack/push :push/exec exec-top)
                   (stack/push :push/char (first s))))))))})

;; =============================================================================
;; Comparison
;; =============================================================================

(register-instruction
 {:sym-name :string_<
  :in [:push/string :push/string]
  :out :push/boolean
  :f (fn [_ s1 s2] (neg? (compare s1 s2)))})

(register-instruction
 {:sym-name :string_>
  :in [:push/string :push/string]
  :out :push/boolean
  :f (fn [_ s1 s2] (pos? (compare s1 s2)))})

;; =============================================================================
;; Trim
;; =============================================================================

(register-instruction
 {:sym-name :string_trim
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (str/trim s))})

(register-instruction
 {:sym-name :string_trim_left
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (str/triml s))})

(register-instruction
 {:sym-name :string_trim_right
  :in [:push/string]
  :out :push/string
  :f (fn [_ s] (str/trimr s))})

;; =============================================================================
;; Random Generation
;; =============================================================================

(register-instruction
 {:sym-name :string_rand
  :in []
  :out :push/string
  :f (fn [_]
       (let [len (rand/rand-int 10)
             chars (repeatedly len #(char (+ 32 (rand/rand-int 95))))]
         (apply str chars)))})

;; =============================================================================
;; Stack Manipulation (standard for all types)
;; =============================================================================

(doseq [[sym-name impl-fn]
        [[:string_stackdepth impl/stack-depth-impl]
         [:string_pop impl/popper]
         [:string_dup impl/dupper]
         [:string_flush impl/flusher]
         [:string_shove impl/shover]
         [:string_yank impl/yanker]
         [:string_yankdup impl/yankdupper]
         [:string_rot impl/rotater]
         [:string_swap impl/swapper]
         [:string_define impl/definer]]]
  (register-instruction
   (impl-fn {:sym-name sym-name
             :push-type :push/string})))
