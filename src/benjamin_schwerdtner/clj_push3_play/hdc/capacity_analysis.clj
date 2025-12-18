(ns benjamin-schwerdtner.clj-push3-play.hdc.capacity-analysis
  "Analysis of Resonator Network capacity limits in FHRR"
  (:require
   [libpython-clj2.python :refer [py. py..] :as py]
   [libpython-clj2.require :refer [require-python]]
   [benjamin-schwerdtner.clj-push3-play.hdc.fhrr :as hd]
   [benjamin-schwerdtner.clj-push3-play.hdc.resonator :as resonator]))

(require-python '[torch :as torch])



;; operational capacity M-max is the max 'problem size' for which the factorization works with high probability.
;; when limited to a max number of iterations `max-iterations`,
;; gives a total accuracy >= p

(defn search-space-size [book-length book-count]
  (Math/pow book-length book-count))

(defn max-iterations [book-length book-count]
  (Math/floor (/ (search-space-size book-length book-count) 1000)))

(defn test-capacity
  "Test resonator capacity for given codebook size m and number of factors n"
  [{:keys [m n d k trials]
    :or {m 10 n 3 d 10000 trials 100
         k (max-iterations m n)}}]
  (binding [hd/*opts* {:fhrr/dimensions d}]
    (let [books (torch/stack (vec (repeatedly n #(hd/seed m))))
          results (for [trial (range trials)]
                    (let [ ;; Select random factors (one from each book)
                          factors (mapv (fn [book]
                                          (py/get-item book (rand-int m)))
                                        books)
                          ;; Create composite by binding
                          x (hd/bind factors)
                          ;; Try to factorize
                          result (resonator/resonator-fhrr x books
                                                           {:max-iterations k
                                                            :similarity-threshold 0.95})]
                      (:success? result)))]
      {:m m
       :n n
       :d d
       :trials trials
       :success-rate (float (/ (count (filter true? results)) trials))
       :failures (count (filter false? results))})))

(defn analyze-capacity-curve
  "Analyze capacity across different codebook sizes"
  [{:keys [n d trials max-m]
    :or {n 3 d 10000 trials 100 max-m 20}}]
  (println "Analyzing Resonator Network Capacity")
  (println (format "Factors: %d, Dimension: %d, Trials per m: %d" n d trials))
  (println "--------------------------------------------")
  (println "m\tSuccess Rate\tFailures")
  (doseq [m (range 2 (inc max-m))]
    (let [result (test-capacity {:m m :n n :d d :trials trials})]
      (println (format "%d\t%.3f\t\t%d"
                       (:m result)
                       (:success-rate result)
                       (:failures result))))))

(defn theoretical-capacity
  "Calculate theoretical capacity based on literature.

  According to Frady et al. 2020:
  - The capacity depends on the ratio of the vector dimension d to the
    product of codebook sizes (m^n for n factors of size m each).
  - The phase space becomes overcrowded when m^n approaches sqrt(d).
  - For reliable factorization, we need m^n << sqrt(d).
  "
  [{:keys [d n]}]
  (let [sqrt-d (Math/sqrt d)
        ;; Conservative estimate: m^n should be < sqrt(d)/10 for reliable operation
        conservative-m (Math/pow (/ sqrt-d 10) (/ 1.0 n))
        ;; Aggressive estimate: m^n should be < sqrt(d)/2
        aggressive-m (Math/pow (/ sqrt-d 2) (/ 1.0 n))]
    {:dimension d
     :factors n
     :sqrt-d sqrt-d
     :conservative-m (int conservative-m)
     :aggressive-m (int aggressive-m)
     :explanation (str "For d=" d " and n=" n " factors:\n"
                       "  Conservative: m ≤ " (int conservative-m)
                       " (m^n=" (int (Math/pow conservative-m n)) " << " (int sqrt-d) ")\n"
                       "  Aggressive: m ≤ " (int aggressive-m)
                       " (m^n=" (int (Math/pow aggressive-m n)) " < " (int sqrt-d) ")")}))

(comment
  ;; Test basic capacity


  (test-capacity {:m 10 :n 3 :d 10000 :trials 10})
  (test-capacity {:m 10 :n 3 :d 50000 :trials 10})

  (time (test-capacity {:m 20 :n 3 :d 50000 :trials 10}))
  (time
   (test-capacity {:m 20 :n 3 :d 1000000 :trials 10}))

  (test-capacity
   {:m 10 :n 3 :d 512 :trials 10
    :k (max-iterations 10 3)})



  (py..
      (binding [hd/*opts* {:fhrr/dimensions (long 1e6)}]
        (hd/seed))
      (size))







  ;; Analyze capacity curve
  (analyze-capacity-curve {:n 3 :d 10000 :trials 50 :max-m 15})

  ;; Calculate theoretical limits
  (theoretical-capacity {:d 10000 :n 3})

  ;; For your specific case with D=10000, n=3:
  ;; sqrt(10000) = 100
  ;; For 3 factors: m^3 should be << 100
  ;; m=3: 3^3 = 27 (should work well)
  ;; m=4: 4^3 = 64 (getting close to limit)
  ;; m=5: 5^3 = 125 (exceeds sqrt(d), expect failures)

  ;; Test with different dimensions
  (doseq [d [1000 5000 10000 20000]]
    (println "\nDimension:" d)
    (println (theoretical-capacity {:d d :n 3}))))






;; Really funny point about resonators (Kent et.al 2020):

;; This implies that factorization is easiest for resonator networks when the decomposition is into three factors, an interesting phenomenon for which we do not have an explanation at this time.








;; --------------------------------------------






;; Spencer J. Kent, E. Paxon Frady, Friedrich T. Sommer, Bruno A. Olshausen; Resonator Networks, 2: Factorization Performance and Capacity Compared to Optimization-Based Methods. Neural Comput 2020; 32 (12): 2332–2388. doi: https://doi.org/10.1162/neco_a_01329
