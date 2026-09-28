;; https://github.com/trystan/random-seed/blob/master/src/random_seed/core.clj
(ns random-seed.core
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle]))

;; Benjamin:
;; I made this a dynamic var
;; https://ask.clojure.org/index.php/4257/clojure-core-rand-for-seedable-randomness
;; I cannot use ThreadLocalRandom unfortunately, the seed cannot be set.

;; Benjamin / note:
;; java.util.Random scrambles its seed weakly. The FIRST `nextFloat` of
;; consecutive small seeds is nearly the same number:
;;
;;   (map #(.nextFloat (doto (java.util.Random.) (.setSeed %))) (range 5))
;;   => (0.73096776 0.7308782 0.73114693 0.73105735 0.7306094)
;;
;; which is fatal when seeds come from a counter, as they do in an
;; evolutionary loop: `(rand-int 8)` returns 5 for every individual in the
;; generation, so every offspring gets the identical "random" mutation.
;;
;; So the seed goes through the SplitMix64 finalizer first. This changes which
;; stream a given seed names - it does not weaken determinism, the same seed
;; still gives the same stream.

(def ^:private ^:const mix-c1 (unchecked-long -7046029254386353131)) ;; 0x9E3779B97F4A7C15
(def ^:private ^:const mix-c2 (unchecked-long -4658895280553007687)) ;; 0xBF58476D1CE4E5B9
(def ^:private ^:const mix-c3 (unchecked-long -7723592293110705685)) ;; 0x94D049BB133111EB

(defn scramble
  "SplitMix64 finalizer: spreads nearby seeds across the whole 64 bit range."
  ^long [^long seed]
  (let [z (unchecked-add seed mix-c1)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 30)) mix-c2)
        z (unchecked-multiply (bit-xor z (unsigned-bit-shift-right z 27)) mix-c3)]
    (bit-xor z (unsigned-bit-shift-right z 31))))

(defn random [^long seed]
  (doto (new java.util.Random) (.setSeed (scramble seed))))

(def ^:dynamic *rng* (random 0))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive). Works like clojure.core/rand except it
  uses the seed specified in set-random-seed!."
  ([] (.nextFloat *rng*))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  Works like clojure.core/rand except it uses the seed specified in
  set-random-seed!."
  [n]
  (int (rand n)))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection. Works like clojure.core/rand except it uses the seed
  specified in set-random-seed!."
  [coll]
  (nth coll (rand-int (count coll))))

(defn shuffle
  "Return a random permutation of coll. Works like clojure.core/shuffle
  except it uses the seed specified in set-random-seed!."
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al *rng*)
    (clojure.lang.RT/vector (.toArray al))))
