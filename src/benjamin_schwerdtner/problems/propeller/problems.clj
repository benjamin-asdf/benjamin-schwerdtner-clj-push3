(ns benjamin-schwerdtner.problems.propeller.problems
  "The Propeller benchmark problems, on this Push3 implementation.

  ## What a problem is

  A map. The five fields that make it the benchmark problem it is, all taken
  from the corresponding file under `propeller_problems/`:

    `:stacks`     the stack set its instruction alphabet is expanded from
    `:ercs`       its literal constants and random-literal generators
    `:arity`      how many `:inN` instructions it gets
    `:output`     the stack the answer is read from - often `:push/print`
    `:error`      how a wrong answer is scored

  and three that define the data:

    `:solution`     the reference implementation, in Clojure
    `:gen-inputs`   one random input tuple
    `:edge-inputs`  input tuples the training set must contain

  `:solution` and `:gen-inputs` replace Propeller's `psb2/fetch-examples`,
  which downloads datasets that are not vendored here. See
  `problems/propeller/data.clj` for what that costs - in short, absolute
  success rates here are not comparable to published PSB2 numbers, while
  comparisons within this repository are fine.

  ## Faithfulness

  Instruction sets, ERCs, output stacks and error metrics are transcribed from
  the Propeller sources and are exact. The reference solutions are written
  from the published problem statements. Where a statement leaves something
  open, the choice made is written into `:notes` next to the problem rather
  than buried - `bouncing-balls` is the one where it materially matters.

  Two things found while transcribing, recorded because they change results:

  - `small-or-large` is not a two-way split. The PSB1 statement has a DEAD
    ZONE: \"small\" below 1000, \"large\" at 2000 and above, and the empty
    string in between. `problems/propeller_adapter.clj` in this repository
    implemented it as small/large/equal about a single 1000 boundary, which is
    an easier and different problem.

  - `shopping-list` gets the stack set `#{:exec :integer :float :boolean
    :print}` in Propeller, but its inputs are two VECTORS of floats. With no
    `:vector_float` in the alphabet a program cannot take them apart, so the
    problem as configured is unsolvable. Propeller's set is reproduced, and
    `:stacks-corrected` carries the set that makes it solvable."
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]
   [benjamin-schwerdtner.problems.propeller.alphabet :as alphabet]
   [benjamin-schwerdtner.problems.propeller.data :as data :refer
    [rand-int-between rand-float-between rand-word rand-words-string
     rand-digit-string rand-vector rand-visible-char]]
   [random-seed.core :refer [rand rand-int rand-nth shuffle]]))

;; ===========================================================================
;; Error metrics
;; ===========================================================================

(defn levenshtein
  "Edit distance between two strings.

  Propeller's `tools.metrics/levenshtein-distance`. This is the error metric
  for every string-output problem in the suite, and the reason those problems
  are learnable at all: it turns \"wrong string\" into a gradient, so a program
  that prints `Fiz` scores better than one that prints nothing."
  [a b]
  (let [a (str a) b (str b)]
    (cond
      (zero? (count a)) (count b)
      (zero? (count b)) (count a)
      :else
      (peek
       (reduce
        (fn [prev i]
          (reduce
           (fn [row j]
             (conj row
                   (if (= (nth a i) (nth b j))
                     (nth prev j)
                     (inc (min (nth prev j)      ;; substitute
                               (nth prev (inc j)) ;; delete
                               (peek row))))))    ;; insert
           [(inc i)]
           (range (count b))))
        (vec (range (inc (count b))))
        (range (count a)))))))

(def no-output-penalty
  "Error charged when a program left nothing on its output stack.

  Propeller uses 1e4 for some problems and 1e6 for others with no stated
  reason; one value is used here so that a problem's difficulty is not
  confounded by its penalty. The magnitude only has to dominate any real
  error, because under lexicase selection what matters is the ORDER of errors
  on a case, not the size."
  1.0e6)

(defn- numeric-error [expected actual]
  (if (number? actual)
    (double (abs (- expected actual)))
    no-output-penalty))

(defn- string-error [expected actual]
  (if (string? actual)
    (double (levenshtein expected actual))
    no-output-penalty))

(defn- exact-error [expected actual]
  (cond
    (= actual :no-stack-item) no-output-penalty
    (= expected actual) 0.0
    :else 1.0))

(defn- bitwise-error
  "One error PER BIT: 0 if that output bit is right, 1 if it is wrong or was
  never written.

  Returns a sequence, which `run/errors` flattens into the error vector - so
  the multiplier benchmarks select on individual bits. There is deliberately
  no large no-output penalty here: an unset register is simply a wrong bit.
  Propeller's comment on this is \"no answer same as wrong answer\", and it is
  the right call, because a program that has correctly computed two of six
  bits should be scored on those two rather than written off."
  [expected actual]
  (if (sequential? actual)
    (mapv (fn [e a] (if (= e a) 0.0 1.0)) expected actual)
    (vec (repeat (count expected) 1.0))))

;; The dispatch from a problem's `:error` keyword to one of these is
;; [[error-fn]] at the bottom of this namespace, which also handles the two
;; problems whose output needs decoding first.

;; ===========================================================================
;; Reference solutions
;;
;; Named rather than inlined where they are worth reading on their own - these
;; are the programs the benchmark is asking to be synthesised.
;; ===========================================================================

(defn small-or-large
  "PSB1. Note the dead zone between 1000 and 2000."
  [x]
  (cond (< x 1000) "small"
        (>= x 2000) "large"
        :else ""))

(defn grade
  "PSB1. `a` `b` `c` `d` are the lower thresholds, `score` the grade.

  Propeller's variant asks only for the letter, not the full sentence."
  [a b c d score]
  (cond (>= score a) "A"
        (>= score b) "B"
        (>= score c) "C"
        (>= score d) "D"
        :else "F"))

(def scrabble-values
  "Scrabble letter values, indexed by letter. Case-insensitive; anything that
  is not a letter scores zero."
  (let [by-letter {1 "aeioulnstr" 2 "dg" 3 "bcmp" 4 "fhvwy" 5 "k"
                   8 "jx" 10 "qz"}]
    (into {} (for [[v letters] by-letter, c letters] [c v]))))

(defn scrabble-score
  [s]
  (reduce + 0 (map #(get scrabble-values (Character/toLowerCase ^char %) 0) s)))

(defn count-odds [v] (count (filter odd? v)))

(defn basement
  "PSB2. First index whose inclusive prefix sum is negative, or -1."
  [v]
  (or (first (keep-indexed (fn [i _] (when (neg? (reduce + (take (inc i) v))) i))
                           v))
      -1))

(defn bouncing-balls
  "PSB2. Total distance travelled over `bounces` bounces.

  The statement gives the bounciness index as `h1 / h0` and then asks for
  \"the total distance the ball travels across those bounces\", which does not
  fix whether the final rise is counted once or twice. Taken here as: the
  initial drop, then for each bounce a full up-and-down at the current height.
  Stated because it is a choice, and because it is the one place in this port
  where a reader should not assume agreement with the official dataset."
  [h0 h1 bounces]
  (let [b (/ h1 h0)]
    (loop [n bounces h h0 total (double h0)]
      (if (zero? n)
        total
        (recur (dec n) (* h b) (+ total (* 2.0 h b)))))))

(defn bowling
  "PSB2. Score a ten-pin game from its roll string.

  `X` is a strike, `/` a spare, `-` a miss, `1`-`9` that many pins."
  [s]
  (let [pins (fn [c] (case c \X 10 \- 0 (- (int c) (int \0))))]
    (loop [i 0 frame 1 total 0]
      (if (or (> frame 10) (>= i (count s)))
        total
        (let [c (nth s i)]
          (cond
            (= c \X)
            (let [b1 (nth s (inc i) nil)
                  b2 (nth s (+ i 2) nil)
                  bonus (+ (if b1 (if (= b1 \/) 10 (pins b1)) 0)
                           (if b2
                             (if (= b2 \/)
                               ;; A spare after a strike: the two bonus rolls
                               ;; together made ten, and b1 already counted
                               ;; the first of them.
                               (- 10 (pins b1))
                               (pins b2))
                             0))]
              (recur (inc i) (inc frame) (+ total 10 bonus)))

            (= (nth s (inc i) nil) \/)
            (let [b (nth s (+ i 2) nil)]
              (recur (+ i 2) (inc frame)
                     (+ total 10 (if b (if (= b \X) 10 (pins b)) 0))))

            :else
            (recur (+ i 2) (inc frame)
                   (+ total (pins c) (pins (nth s (inc i) \-))))))))))

(defn camel-case
  "PSB2. kebab-case to camelCase."
  [s]
  (let [[head & tail] (string/split s #"-" -1)]
    (apply str head (map string/capitalize tail))))

(defn dice-game
  "PSB2. P(Peter's n-sided die strictly beats Colin's m-sided die)."
  [n m]
  (/ (double (reduce + (for [p (range 1 (inc n))]
                         (count (filter #(< % p) (range 1 (inc m)))))))
     (* (double n) (double m))))

(defn find-pair
  "PSB2. The first two integers, in order, that sum to `target`.

  Two outputs, which is why this problem needs its own output handling."
  [v target]
  (or (first (for [i (range (count v))
                   j (range (inc i) (count v))
                   :when (= target (+ (nth v i) (nth v j)))]
               [(nth v i) (nth v j)]))
      [0 0]))

(defn fizz-buzz [x]
  (cond (zero? (mod x 15)) "FizzBuzz"
        (zero? (mod x 3)) "Fizz"
        (zero? (mod x 5)) "Buzz"
        :else (str x)))

(defn fuel-cost [v] (reduce + (map #(- (quot % 3) 2) v)))

(defn gcd [a b] (loop [a a b b] (if (zero? b) a (recur b (mod a b)))))

(defn luhn
  "PSB2. Double every other digit starting from the second, casting out nines,
  and sum."
  [v]
  (reduce + (map-indexed (fn [i d]
                           (if (odd? i)
                             (let [d2 (* 2 d)] (if (> d2 9) (- d2 9) d2))
                             d))
                         v)))

(defn middle-character [s]
  (let [n (count s) mid (quot n 2)]
    (if (odd? n)
      (subs s mid (inc mid))
      (subs s (dec mid) (inc mid)))))

(defn paired-digits
  "PSB2. Sum of digits whose successor is the same digit."
  [s]
  (reduce + (map (fn [a b] (if (= a b) (- (int a) (int \0)) 0))
                 s (rest s))))

(defn shopping-list
  "PSB2. Total after applying each item's percent discount."
  [prices discounts]
  (reduce + (map (fn [p d] (* p (- 1.0 (/ d 100.0)))) prices discounts)))

(defn snow-day
  "PSB2. Snow on the ground after `hours`, given a fall rate and a melt
  proportion applied each hour."
  [hours snow rate melt]
  (loop [h hours s (double snow)]
    (if (zero? h) s (recur (dec h) (* (+ s rate) (- 1.0 melt))))))

(defn solve-boolean
  "PSB2. Evaluate a `t`/`f`/`&`/`|` expression, left to right, no precedence."
  [s]
  (loop [[c & more] (seq s) acc nil op nil]
    (if (nil? c)
      (boolean acc)
      (case c
        \t (recur more (case op nil true \& (and acc true) \| (or acc true)) nil)
        \f (recur more (case op nil false \& (and acc false) \| (or acc false)) nil)
        (recur more acc c)))))

(defn spin-words
  "PSB2. Reverse every word of five or more letters."
  [s]
  (string/join " " (map #(if (<= 5 (count %)) (string/reverse %) %)
                        (string/split s #" " -1))))

(defn square-digits
  "PSB2. Square each digit and concatenate the results."
  [x]
  (apply str (map #(let [d (- (int %) (int \0))] (* d d)) (str x))))

(defn substitution-cipher
  "PSB2. `from` and `to` define a character mapping; apply it to `text`."
  [from to text]
  (let [m (zipmap from to)]
    (apply str (map #(get m % %) text))))

(defn twitter
  "PSB2. Report a tweet's length, or complain."
  [s]
  (cond (zero? (count s)) "You didn't type anything"
        (> (count s) 140) "Too many characters"
        :else (str "your tweet has " (count s) " characters")))

;; ===========================================================================
;; The registry
;; ===========================================================================

(def random-int
  "Propeller's integer ERC: a random integer in [-100, 100]. Shared by most
  of the suite."
  (fn [] (rand-int-between -100 100)))

(defn- interpose-ops
  "`operands` with `operators` woven between them: a b c, & | -> a & b | c."
  [operands operators]
  (cons (first operands)
        (mapcat vector operators (rest operands))))

(def psb1
  [{:name :small-or-large
    :suite :psb1
    :source "propeller_problems/problems/PSB1/small_or_large.cljc"
    :description "\"small\" below 1000, \"large\" from 2000 up, \"\" between."
    :arity 1
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :string :print}
    :ercs ["" "small" "large" random-int]
    :solution small-or-large
    :gen-inputs #(vector (rand-int-between -10000 10000))
    ;; The thresholds and their neighbours. Without these an i.i.d. sample
    ;; over [-10000, 10000] puts ~5% of cases in the dead zone and a program
    ;; that never prints "" scores well on the training set.
    :edge-inputs [[0] [999] [1000] [1001] [1999] [2000] [2001] [-10000] [10000]]
    :difficulty :easy}

   {:name :grade
    :suite :psb1
    :source "propeller_problems/problems/PSB1/grade.cljc"
    :description "Four descending thresholds and a score; print the letter."
    :arity 5
    :output :push/print
    :error :levenshtein
    :stacks #{:boolean :exec :integer :string :print}
    :ercs ["A" "B" "C" "D" "F" random-int]
    :solution grade
    :gen-inputs (fn []
                  ;; Four distinct thresholds, descending, as the statement
                  ;; requires - an unsorted tuple is not a valid case.
                  (let [[a b c d] (reverse (sort (take 4 (shuffle (range 0 101)))))]
                    [a b c d (rand-int-between 0 100)]))
    :edge-inputs [[90 80 70 60 100] [90 80 70 60 90] [90 80 70 60 89]
                  [90 80 70 60 60] [90 80 70 60 59] [90 80 70 60 0]]
    :difficulty :medium}

   {:name :count-odds
    :suite :psb1
    :source "propeller_problems/problems/PSB1/count_odds.cljc"
    :description "Count the odd integers in a vector."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :vector_integer :print}
    :ercs [random-int 0 1 2]
    :solution count-odds
    :gen-inputs (fn [] [(rand-vector 0 20 (fn [] (rand-int-between -100 100)))])
    :edge-inputs [[[]] [[0]] [[1]] [[-1]] [[2 4 6 8]] [[1 3 5 7 9]]]
    :difficulty :medium}

   {:name :scrabble-score
    :suite :psb1
    :source "propeller_problems/problems/PSB1/scrabble_score.cljc"
    :description "Sum the Scrabble values of a string's letters."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [rand-visible-char 0 1]
    :solution scrabble-score
    :gen-inputs #(vector (apply str (repeatedly (rand-int-between 0 20)
                                                rand-visible-char)))
    :edge-inputs [[""] ["a"] ["z"] ["q"] ["!"] ["aaaaaaaaaaaaaaaaaaaa"]]
    :difficulty :hard}])

(def psb2
  [{:name :basement
    :suite :psb2
    :source "propeller_problems/problems/PSB2/basement.cljc"
    :description "First index whose prefix sum is negative."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :vector_integer :print}
    :ercs [random-int -1 0 1 []]
    :solution basement
    ;; Rejection sampling: the specification guarantees a negative prefix sum
    ;; exists, so a draw without one is not a valid case and is redrawn.
    :gen-inputs (fn []
                  (loop []
                    (let [v (rand-vector 1 20 #(rand-int-between -100 100))]
                      (if (neg? (basement v)) (recur) [v]))))
    :edge-inputs [[[-1]] [[1 -2]] [[-100]] [[1 1 1 -100]]]
    :difficulty :medium}

   {:name :bouncing-balls
    :suite :psb2
    :source "propeller_problems/problems/PSB2/bouncing_balls.cljc"
    :description "Total distance a bouncing ball travels."
    :arity 3
    :output :push/float
    :error :numeric
    :selection :epsilon-lexicase
    :stacks #{:exec :integer :float :boolean :print}
    :ercs [0.0 1.0 2.0]
    :solution bouncing-balls
    :gen-inputs (fn []
                  (let [h0 (rand-float-between 1.0 100.0)
                        h1 (rand-float-between 0.01 (- h0 0.01))]
                    [h0 h1 (rand-int-between 1 20)]))
    :edge-inputs [[100.0 1.0 1] [1.0 0.5 1] [100.0 50.0 20]]
    :notes "The reference counts the initial drop plus a full up-and-down per
            bounce; see [[bouncing-balls]]. The statement does not fix this."
    :difficulty :hard}

   {:name :bowling
    :suite :psb2
    :source "propeller_problems/problems/PSB2/bowling.cljc"
    :description "Score a ten-pin bowling game from its roll string."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [\- \X \/ \1 \2 \3 \4 \5 \6 \7 \8 \9 10 random-int]
    :solution bowling
    :gen-inputs (fn []
                  ;; Build a legal game frame by frame, which is the only way
                  ;; to get a valid roll string: a spare cannot open a frame
                  ;; and two rolls in a frame cannot exceed nine pins.
                  (let [frame
                        (fn []
                          (let [r (rand)]
                            (cond
                              (< r 0.3) "X"
                              (< r 0.6) (str (rand-int-between 0 9) "/")
                              :else
                              (let [a (rand-int-between 0 9)
                                    b (rand-int-between 0 (- 9 a))]
                                (str (if (zero? a) "-" a)
                                     (if (zero? b) "-" b))))))]
                    [(apply str (repeatedly 10 frame))]))
    :edge-inputs [["XXXXXXXXXXXX"] ["--------------------"]
                  ["9-9-9-9-9-9-9-9-9-9-"] ["5/5/5/5/5/5/5/5/5/5/5"]]
    :difficulty :very-hard}

   {:name :camel-case
    :suite :psb2
    :source "propeller_problems/problems/PSB2/camel_case.cljc"
    :description "kebab-case to camelCase."
    :arity 1
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [\- \space rand-visible-char (fn [] (rand-word 1 21))]
    :solution camel-case
    :gen-inputs #(vector (rand-words-string (rand-int-between 1 5) 1 8 "-"))
    :edge-inputs [["a"] ["a-b"] ["a-b-c"] [""] ["hello-world"]]
    :difficulty :hard}

   {:name :dice-game
    :suite :psb2
    :source "propeller_problems/problems/PSB2/dice_game.cljc"
    :description "P(an n-sided die beats an m-sided die)."
    :arity 2
    :output :push/float
    :error :numeric
    :selection :epsilon-lexicase
    :stacks #{:exec :integer :float :boolean :print}
    :ercs [0.0 1.0]
    :solution dice-game
    :gen-inputs #(vector (rand-int-between 1 1000) (rand-int-between 1 1000))
    :edge-inputs [[1 1] [1 1000] [1000 1] [6 6] [2 1]]
    :difficulty :hard}

   {:name :find-pair
    :suite :psb2
    :source "propeller_problems/problems/PSB2/find_pair.cljc"
    :description "The first two integers summing to the target."
    :arity 2
    ;; The only two-output problem in the suite: the answer is the top TWO
    ;; items of the INTEGER stack, so it needs its own reader. See
    ;; [[read-output]].
    :output :push/integer-pair
    :error :numeric-pair
    :stacks #{:exec :integer :vector_integer :boolean}
    :ercs [-1 0 1 2 random-int]
    :solution find-pair
    :gen-inputs (fn []
                  ;; Draw the vector, then pick the target from a pair that is
                  ;; actually in it, so a solution always exists.
                  (loop []
                    (let [v (rand-vector 2 20 #(rand-int-between -100 100))
                          i (rand-int (count v))
                          j (rand-int (count v))]
                      (if (= i j)
                        (recur)
                        [v (+ (nth v i) (nth v j))]))))
    :edge-inputs [[[1 2] 3] [[0 0] 0] [[-1 1] 0] [[100 -100] 0]]
    :difficulty :hard}

   {:name :fizz-buzz
    :suite :psb2
    :source "propeller_problems/problems/PSB2/fizz_buzz.cljc"
    :description "Fizz on 3, Buzz on 5, FizzBuzz on both, else the number."
    :arity 1
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :string :print}
    :ercs [3 5 "Fizz" "Buzz" "FizzBuzz" random-int]
    :solution fizz-buzz
    :gen-inputs #(vector (rand-int-between 1 1000))
    :edge-inputs [[1] [3] [5] [15] [30] [9] [25]]
    :difficulty :medium}

   {:name :fuel-cost
    :suite :psb2
    :source "propeller_problems/problems/PSB2/fuel_cost.cljc"
    :description "Sum of (each element divided by three, floored, minus two)."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :vector_integer :print}
    :ercs [random-int 0 1 2 3]
    :solution fuel-cost
    :gen-inputs (fn [] [(rand-vector 1 20 (fn [] (rand-int-between 6 100000)))])
    :edge-inputs [[[6]] [[14]] [[1969]] [[100756]] [[6 6 6]]]
    :difficulty :easy}

   {:name :gcd
    :suite :psb2
    :source "propeller_problems/problems/PSB2/gcd.cljc"
    :description "Greatest common divisor of two positive integers."
    :arity 2
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :print}
    :ercs [random-int]
    :solution gcd
    :gen-inputs #(vector (rand-int-between 1 1000000) (rand-int-between 1 1000000))
    :edge-inputs [[1 1] [1 1000000] [7 7] [12 18] [1000000 1000000]]
    :difficulty :hard}

   {:name :luhn
    :suite :psb2
    :source "propeller_problems/problems/PSB2/luhn.cljc"
    :description "Luhn's checksum over sixteen digits."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :vector_integer :print}
    :ercs [0 2 9 10 random-int]
    :solution luhn
    :gen-inputs (fn [] [(vec (repeatedly 16 (fn [] (rand-int-between 1 9))))])
    :edge-inputs [[(vec (repeat 16 1))] [(vec (repeat 16 9))]
                  [(vec (repeat 16 5))]]
    :difficulty :hard}

   {:name :middle-character
    :suite :psb2
    :source "propeller_problems/problems/PSB2/middle_character.cljc"
    :description "The middle character, or the middle two if even length."
    :arity 1
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs ["" 0 1 2 random-int]
    :solution middle-character
    :gen-inputs #(vector (apply str (repeatedly (rand-int-between 1 20)
                                                rand-visible-char)))
    :edge-inputs [["a"] ["ab"] ["abc"] ["abcd"]]
    :difficulty :medium}

   {:name :paired-digits
    :suite :psb2
    :source "propeller_problems/problems/PSB2/paired_digits.cljc"
    :description "Sum of digits whose following digit is the same."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [0 random-int rand-visible-char]
    :solution paired-digits
    :gen-inputs (fn []
                  ;; Runs of repeated digits, deliberately: a uniform digit
                  ;; string almost never has an adjacent pair, and a training
                  ;; set of zeros teaches nothing.
                  [(apply str (repeatedly (rand-int-between 1 10)
                                          #(apply str
                                                  (repeat (rand-int-between 1 3)
                                                          (rand-digit-string 1)))))])
    :edge-inputs [["11"] ["12"] ["99"] ["1111"] ["123456789"]]
    :difficulty :medium}

   {:name :shopping-list
    :suite :psb2
    :source "propeller_problems/problems/PSB2/shopping_list.cljc"
    :description "Total price after each item's percent discount."
    :arity 2
    :output :push/float
    :error :numeric
    :selection :epsilon-lexicase
    ;; Propeller's set, reproduced although it makes the problem unsolvable -
    ;; see the namespace docstring.
    :stacks #{:exec :integer :float :boolean :print}
    :stacks-corrected #{:exec :integer :float :boolean :vector_float :print}
    :ercs [0.0 100.0 (fn [] (rand-float-between 0.0 100.0))]
    :solution shopping-list
    :gen-inputs (fn []
                  (let [n (rand-int-between 1 20)]
                    [(vec (repeatedly n #(rand-float-between 0.0 50.0)))
                     (vec (repeatedly n #(rand-float-between 0.0 100.0)))]))
    :edge-inputs [[[1.0] [0.0]] [[1.0] [100.0]] [[10.0 10.0] [50.0 50.0]]]
    :notes "Propeller's stack set has no :vector_float, so a program cannot
            read either input. :stacks-corrected adds it."
    :difficulty :hard}

   {:name :snow-day
    :suite :psb2
    :source "propeller_problems/problems/PSB2/snow_day.cljc"
    :description "Snow on the ground after n hours of fall and melt."
    :arity 4
    :output :push/float
    :error :numeric
    :selection :epsilon-lexicase
    :stacks #{:exec :integer :float :boolean :print}
    :ercs [0 1 -1 0.0 1.0 -1.0]
    :solution snow-day
    :gen-inputs #(vector (rand-int-between 0 20)
                         (rand-float-between 0.0 20.0)
                         (rand-float-between 0.0 10.0)
                         (rand-float-between 0.0 1.0))
    :edge-inputs [[0 0.0 0.0 0.0] [1 1.0 1.0 0.0] [20 0.0 1.0 1.0]]
    :difficulty :hard}

   {:name :solve-boolean
    :suite :psb2
    :source "propeller_problems/problems/PSB2/solve_boolean.cljc"
    :description "Evaluate a t/f expression with & and |, left to right."
    :arity 1
    :output :push/boolean
    :error :exact
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [true false \t \f \& \|]
    :solution solve-boolean
    :gen-inputs (fn []
                  ;; n operands joined by n-1 operators.
                  (let [n (rand-int-between 1 6)]
                    [(apply str
                            (interpose-ops
                             (repeatedly n #(rand-nth "tf"))
                             (repeatedly (dec n) #(rand-nth "&|"))))]))
    :edge-inputs [["t"] ["f"] ["t&f"] ["t|f"] ["t&t&t"] ["f|f|t"]]
    :difficulty :hard}

   {:name :spin-words
    :suite :psb2
    :source "propeller_problems/problems/PSB2/spin_words.cljc"
    :description "Reverse every word of five or more letters."
    :arity 1
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [4 5 \space rand-visible-char (fn [] (rand-word 0 20))]
    :solution spin-words
    :gen-inputs #(vector (rand-words-string (rand-int-between 1 5) 1 8 " "))
    :edge-inputs [["a"] ["hello"] ["abcd"] ["abcde"] ["hello world"]]
    :difficulty :hard}

   {:name :square-digits
    :suite :psb2
    :source "propeller_problems/problems/PSB2/square_digits.cljc"
    :description "Square each digit and concatenate."
    :arity 1
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs ["" 0 1 2 random-int]
    :solution square-digits
    :gen-inputs #(vector (rand-int-between 0 1000000))
    :edge-inputs [[0] [1] [9] [123] [999999]]
    :difficulty :medium}

   {:name :substitution-cipher
    :suite :psb2
    :source "propeller_problems/problems/PSB2/substitution_cipher.cljc"
    :description "Apply a character substitution given as two strings."
    :arity 3
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [0 ""]
    :solution substitution-cipher
    :gen-inputs (fn []
                  (let [n (rand-int-between 1 10)
                        from (take n (shuffle data/lowercase-letters))
                        to (take n (shuffle data/lowercase-letters))]
                    [(apply str from)
                     (apply str to)
                     (apply str (repeatedly (rand-int-between 0 20)
                                            #(rand-nth from)))]))
    :edge-inputs [["a" "b" "a"] ["ab" "ba" "ab"] ["a" "b" ""]]
    :difficulty :medium}

   {:name :twitter
    :suite :psb2
    :source "propeller_problems/problems/PSB2/twitter.cljc"
    :description "Report a tweet's length, or complain about it."
    :arity 1
    :output :push/string
    :error :levenshtein
    :stacks #{:exec :integer :boolean :char :string :print}
    :ercs [0 140 "Too many characters" "You didn't type anything"
           "your tweet has " " characters"]
    :solution twitter
    :gen-inputs #(vector (apply str (repeatedly (rand-int-between 0 200)
                                                rand-visible-char)))
    :edge-inputs [[""] ["a"] [(apply str (repeat 140 \a))]
                  [(apply str (repeat 141 \a))]]
    :difficulty :medium}])

;; ---------------------------------------------------------------------------
;; The self-contained problems - the ones Propeller defines without psb2, so
;; these are exact ports including their data.
;; ---------------------------------------------------------------------------

(def others
  [{:name :smallest
    :suite :software
    :source "propeller_problems/problems/software/smallest.cljc"
    :description "The smallest of four integers, printed."
    :arity 4
    :output :push/print
    ;; Propeller scores this 0 or 1 on string equality of the printed output,
    ;; not by distance - see its error-function.
    :error :exact
    :stacks #{:boolean :exec :integer :print}
    :ercs [random-int]
    :solution (fn [a b c d] (str (min a b c d)))
    :gen-inputs (fn [] (vec (repeatedly 4 (fn [] (rand-int-between -100 100)))))
    :edge-inputs [[0 0 0 0] [-100 100 100 100] [1 2 3 4] [4 3 2 1]]
    :difficulty :easy}

   {:name :number-io
    :suite :software
    :source "propeller_problems/problems/software/number_io.cljc"
    :description "Print the sum of a float and an integer, as a float."
    :arity 2
    :output :push/print
    :error :numeric-printed
    :stacks #{:exec :integer :float :print}
    :ercs []
    :solution (fn [f i] (+ f i))
    :gen-inputs #(vector (rand-float-between -100.0 100.0)
                         (rand-int-between -100 100))
    :edge-inputs [[0.0 0] [-100.0 100] [100.0 -100]]
    :difficulty :easy}

   {:name :string-classification
    :suite :propel
    :source "propeller_problems/problems/string_classification.cljc"
    :description "True if the string contains all of A, C, G and T."
    :arity 1
    :output :push/boolean
    :error :exact
    ;; This one does NOT use get-stack-instructions: it carries the original
    ;; propel instruction list, a deliberately tiny alphabet. Reproduced as
    ;; given, which is why :stacks is absent.
    :instructions [:integer_+ :integer_- :integer_* :integer_quot :integer_=
                   :exec_dup :exec_if
                   :boolean_and :boolean_or :boolean_not :boolean_=
                   :string_= :string_take :string_drop :string_reverse
                   :string_concat :string_length :string_contains]
    :ercs [0 1 true false "" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "A" "C" "G" "T"]
    :solution (fn [s] (every? #(string/includes? s (str %)) "ACGT"))
    :gen-inputs (fn [] [(apply str (repeatedly (rand-int-between 0 8)
                                               (fn [] (rand-nth "ACGT"))))])
    :edge-inputs [["GCG"] ["GACAG"] ["AGAAG"] ["CCCA"] ["GATTACA"] ["TAGG"]
                  ["GACT"]]
    :difficulty :easy}

   {:name :integer-regression
    :suite :regression
    :source "propeller_problems/problems/regression/integer_regression.cljc"
    :description "f(x) = x^3 + 2x^2 + x + 3."
    :arity 1
    :output :push/integer
    :error :numeric
    :stacks #{:exec :integer}
    :ercs [random-int]
    :solution (fn [x] (+ (* x x x) (* 2 x x) x 3))
    :gen-inputs #(vector (rand-int-between -10 10))
    :edge-inputs (mapv vector (range -10 11))
    :difficulty :easy}

   {:name :nguyen-f1
    :suite :regression
    :source "propeller_problems/problems/regression/nguyen_f1.cljc"
    :description "f(x) = x^3 + x^2 + x."
    :arity 1
    :output :push/float
    :error :numeric
    :selection :epsilon-lexicase
    :stacks #{:exec :float}
    :ercs [0.0 1.0 -1.0 2.0]
    :solution (fn [x] (+ (* x x x) (* x x) x))
    :gen-inputs #(vector (rand-float-between -4.0 4.0))
    :edge-inputs (mapv (fn [i] [(* 0.5 i)]) (range -8 9))
    :difficulty :medium}

   {:name :nguyen-f5
    :suite :regression
    :source "propeller_problems/problems/regression/nguyen_f5.cljc"
    :description "f(x) = sin(x^2) * cos(x) - 1."
    :arity 1
    :output :push/float
    :error :numeric
    :selection :epsilon-lexicase
    :stacks #{:exec :float}
    :ercs [0.0 1.0 -1.0 2.0]
    :solution (fn [x] (- (* (Math/sin (* x x)) (Math/cos x)) 1))
    :gen-inputs #(vector (rand-float-between -2.0 2.0))
    :edge-inputs (mapv (fn [i] [(* 0.25 i)]) (range -8 9))
    :difficulty :hard}])

;; ---------------------------------------------------------------------------
;; Boolean circuit synthesis
;;
;; The two multipliers are the odd ones out in this suite and it is worth
;; saying how. Every other problem reads its inputs with `:inN` and leaves one
;; answer on a stack. These name their inputs after the BITS they are - `:a2`
;; is the high bit of the first operand - and produce six or eight independent
;; outputs, written into registers with `:set_cN`.
;;
;; The instruction set is not derived from a stack set either; it is the
;; hand-picked gate alphabet from Kalkreuth et al. that Propeller uses (BUFa
;; and NOTa commented out there, with `:boolean_not` added to compensate), and
;; it is reproduced exactly. No `close`, no ERCs, not even `true`/`false` -
;; a circuit is built from its inputs and its gates.
;; ---------------------------------------------------------------------------

(defn- product-bits
  "The product of two unsigned binary numbers, as `n-bits` booleans, MSB
  first."
  [a b n-bits]
  (let [p (* a b)]
    (mapv (fn [i] (bit-test p i)) (reverse (range n-bits)))))

(defn- bits->int
  "Booleans, MSB first, as an unsigned integer."
  [bs]
  (reduce (fn [acc b] (+ (* 2 acc) (if b 1 0))) 0 bs))

(defn mul3
  "3-bit multiplier: six input bits, a six-bit product."
  [a2 a1 a0 b2 b1 b0]
  (product-bits (bits->int [a2 a1 a0]) (bits->int [b2 b1 b0]) 6))

(defn mul4
  "4-bit multiplier: eight input bits, an eight-bit product."
  [a3 a2 a1 a0 b3 b2 b1 b0]
  (product-bits (bits->int [a3 a2 a1 a0]) (bits->int [b3 b2 b1 b0]) 8))

(def ^:private circuit-gates
  ;; Kalkreuth et al.'s recommended set, as Propeller configures it.
  [:boolean_and :boolean_or :boolean_xor
   :boolean_nand :boolean_nor :boolean_xnor
   :boolean_not
   :boolean_dup :boolean_swap :boolean_rot])

(defn- all-bit-tuples
  "Every assignment of `n` bits, which is the complete training set - a
  circuit benchmark has no held-out cases because there is nothing to hold
  out. Propeller's `:test` is empty for both multipliers."
  [n]
  (mapv vec (apply combo/cartesian-product (repeat n [false true]))))

(def circuits
  [{:name :mul3
    :suite :boolean
    :source "propeller_problems/problems/boolean/mul3.cljc"
    :description "3-bit binary multiplier: 6 input bits, 6 output bits."
    :arity 6
    :output :push/output
    :num-outputs 6
    :error :bitwise
    :input-names [:a2 :a1 :a0 :b2 :b1 :b0]
    :instructions (into [:a2 :a1 :a0 :b2 :b1 :b0
                         :set_c5 :set_c4 :set_c3 :set_c2 :set_c1 :set_c0
                         :c5 :c4 :c3 :c2 :c1 :c0]
                        circuit-gates)
    :ercs []
    :solution mul3
    :exhaustive? true
    :gen-inputs (fn [] (rand-nth (all-bit-tuples 6)))
    :edge-inputs (all-bit-tuples 6)
    :difficulty :very-hard}

   {:name :mul4
    :suite :boolean
    :source "propeller_problems/problems/boolean/mul4.cljc"
    :description "4-bit binary multiplier: 8 input bits, 8 output bits."
    :arity 8
    :output :push/output
    :num-outputs 8
    :error :bitwise
    :input-names [:a3 :a2 :a1 :a0 :b3 :b2 :b1 :b0]
    :instructions (into [:a3 :a2 :a1 :a0 :b3 :b2 :b1 :b0
                         :set_c7 :set_c6 :set_c5 :set_c4
                         :set_c3 :set_c2 :set_c1 :set_c0
                         :c7 :c6 :c5 :c4 :c3 :c2 :c1 :c0]
                        circuit-gates)
    :ercs []
    :solution mul4
    :exhaustive? true
    :gen-inputs (fn [] (rand-nth (all-bit-tuples 8)))
    :edge-inputs (all-bit-tuples 8)
    :difficulty :very-hard}])

(def all-problems (vec (concat psb1 psb2 others circuits)))

(def by-name (into {} (map (juxt :name identity)) all-problems))

;; ===========================================================================
;; Turning a problem into something runnable
;; ===========================================================================

(defn alphabet
  "The instruction alphabet for `problem`: its stack set expanded, plus its
  input instructions, plus its ERCs.

  Pass `:corrected? true` to use `:stacks-corrected` where a problem has one."
  ([problem] (alphabet problem {}))
  ([{:keys [stacks stacks-corrected instructions arity ercs input-names]}
    {:keys [corrected?]}]
   (into (vec (concat (or instructions
                          (alphabet/get-stack-instructions
                           (or (when corrected? stacks-corrected) stacks)))
                      ;; A problem that names its inputs already lists those
                      ;; names in its `:instructions`, so `:inN` would only
                      ;; add `arity` instructions that can never resolve.
                      (when-not input-names
                        (map #(keyword (str "in" (inc %))) (range arity)))))
         ercs)))

(defn atom-generators
  "What `generator/generate-code` draws this problem's genome atoms from."
  ([problem] (atom-generators problem {}))
  ([problem opts] (alphabet/atom-generators (alphabet problem opts))))

(defn error-fn
  "`(f expected actual)` -> error, for `problem`."
  [{:keys [error]}]
  (case error
    :numeric numeric-error
    :levenshtein string-error
    :exact exact-error
    :bitwise bitwise-error
    ;; number-io prints its answer, so the printed string has to be read back
    ;; as a number before it can be compared.
    :numeric-printed (fn [expected actual]
                       (let [v (try (Double/parseDouble (string/trim (str actual)))
                                    (catch Exception _ nil))]
                         (if v (numeric-error expected v) no-output-penalty)))
    ;; find-pair: two outputs, error is the sum of the two distances.
    :numeric-pair (fn [expected actual]
                    (if (and (vector? actual) (= 2 (count actual))
                             (every? number? actual))
                      (+ (numeric-error (first expected) (first actual))
                         (numeric-error (second expected) (second actual)))
                      no-output-penalty))))

(defn train-and-test
  "`{:train [...] :test [...]}` for `problem`."
  ([problem] (data/train-and-test problem))
  ([problem n-train n-test seed]
   (data/train-and-test problem n-train n-test seed)))

(comment
  (count all-problems)
  (map :name all-problems)

  ;; What alphabet a problem actually gets, and how big it is.
  (count (alphabet (by-name :fizz-buzz)))
  (take 20 (alphabet (by-name :fizz-buzz)))

  ;; The reference solution against its own edge cases.
  (let [p (by-name :bowling)]
    (mapv (fn [i] [i (apply (:solution p) i)]) (:edge-inputs p))))
