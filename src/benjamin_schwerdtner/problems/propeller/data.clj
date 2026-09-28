(ns benjamin-schwerdtner.problems.propeller.data
  "Random input generators for the PSB1/PSB2 benchmark problems.

  ## Why these exist

  Propeller does not define its PSB1/PSB2 inputs. It calls

      (psb2/fetch-examples \"data\" \"fuel-cost\" 200 2000)

  which downloads the official datasets published alongside Helmuth & Kelly's
  PSB2 paper - a fixed 200 training and 2000 test cases per problem, the same
  bytes for everyone. Those datasets are not vendored in this repository and
  fetching them needs network access, so the inputs are SAMPLED HERE from the
  published input specifications instead.

  ## What that costs, stated plainly

  Absolute success rates measured against these generators are NOT comparable
  to published PSB2 numbers. The distributions follow the specification, but:

  - the official training sets are curated - they include hand-chosen EDGE
    CASES (empty vector, length-1 string, the exact threshold value) at a
    frequency no i.i.d. sampler reproduces, and that curation is a large part
    of what makes a PSB2 training set trainable;
  - `basement` and `find-pair` are specified with a guarantee about the input
    (a negative prefix sum exists; a pair summing to the target exists), which
    a sampler has to enforce by rejection, and the resulting distribution is
    not the published one.

  Each problem's known edge cases are therefore listed explicitly and always
  included in the training set, which is the same trick the official sets use.
  Comparisons WITHIN this repository - baseline GP against metaevolution on
  the same generator and seed - are unaffected, and that is what these are for.

  All draws go through `random-seed.core`, so a problem's cases are a pure
  function of its seed."
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require
   [clojure.string :as string]
   [random-seed.core :as rs :refer [rand rand-int rand-nth shuffle]]))

;; ---------------------------------------------------------------------------
;; Primitive draws
;; ---------------------------------------------------------------------------

(defn rand-int-between
  "A random integer in [lo, hi], inclusive."
  [lo hi]
  (+ lo (rand-int (inc (- hi lo)))))

(defn rand-float-between
  [lo hi]
  (+ lo (* (rand) (- hi lo))))

(def visible-chars
  "The printable ASCII range, which is what PSB2 means by a visible character."
  (mapv char (range 32 127)))

(defn rand-visible-char [] (rand-nth visible-chars))

(def lowercase-letters (mapv char (range (int \a) (inc (int \z)))))

(defn rand-word
  "A random lowercase word of length [lo, hi]."
  [lo hi]
  (apply str (repeatedly (rand-int-between lo hi) #(rand-nth lowercase-letters))))

(defn rand-words-string
  "`n` random words joined by `sep`."
  [n lo hi sep]
  (string/join sep (repeatedly n #(rand-word lo hi))))

(defn rand-digit-string
  [len]
  (apply str (repeatedly len #(char (+ (int \0) (rand-int 10))))))

(defn rand-vector
  [lo-len hi-len draw]
  (vec (repeatedly (rand-int-between lo-len hi-len) draw)))

;; ---------------------------------------------------------------------------
;; Assembling a case set
;; ---------------------------------------------------------------------------

(defn cases
  "`n` cases for `problem`, as `[{:inputs [..] :output x} ...]`.

  The problem supplies `:gen-inputs` (a thunk drawing one input tuple),
  `:solution` (the reference implementation, applied to the tuple) and
  `:edge-inputs` (tuples that must appear). Edge cases come first and count
  against `n`, mirroring how the official training sets are built."
  [{:keys [gen-inputs solution edge-inputs exhaustive?]} n seed]
  (binding [rs/*rng* (rs/random seed)]
    (let [;; An `exhaustive?` problem's input space is finite and small, and
          ;; `:edge-inputs` IS the whole of it - every assignment of six or
          ;; eight bits for the multipliers. Sampling `n` of those would
          ;; either duplicate cases or, worse, leave some input combination
          ;; untested, and a circuit is only correct if it is correct on all
          ;; of them. Propeller's multiplier files build the same complete set
          ;; and leave `:test` empty.
          edges (vec (if exhaustive? edge-inputs (take n edge-inputs)))
          drawn (if exhaustive?
                  ()
                  (repeatedly (max 0 (- n (count edges))) gen-inputs))]
      (mapv (fn [inputs]
              {:inputs (vec inputs)
               :output (apply solution inputs)})
            (concat edges drawn)))))

(defn train-and-test
  "A training and a test set, drawn from disjoint seeds.

  Propeller's default split is 200 train / 2000 test. The test set gets no
  edge cases - it is there to measure generalisation, and handing it the same
  curated cases the training set has would flatter it."
  ([problem] (train-and-test problem 200 2000 0))
  ([problem n-train n-test seed]
   {:train (cases problem n-train seed)
    :test (cases (dissoc problem :edge-inputs) n-test (+ seed 1000000))}))
