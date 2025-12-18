(ns benjamin-schwerdtner.problems.arc-2.data
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.data.json :as json]))

(def training-tasks
  (str/split-lines
   (slurp "../ARC-AGI-2/data/training.txt")))

(def evaluation-tasks
  (str/split-lines
   (slurp "../ARC-AGI-2/data/evaluation.txt")))

(take 5 training-tasks)
'("a85d4709" "c8cbb738" "8e1813be" "a699fb00" "20fb2937")

(defn read-task [train-or-eval task]
  (let [file (io/file (str "../ARC-AGI-2/data/" ({:eval "evaluation" :train "training"} train-or-eval)) (str task ".json"))]
    (assoc
     (json/read
      (io/reader (io/file (str "../ARC-AGI-2/data/" ({:eval "evaluation" :train "training"} train-or-eval)) (str task ".json")))
      {:key-fn keyword})
     :file (str file))))

(defn task-3x3? [task]
  (let [examples (concat (:train task) (:test task))]
    (every? (fn [example]
              (and (= 3 (count (:input example)))
                   (= 3 (count (first (:input example))))
                   (= 3 (count (:output example)))
                   (= 3 (count (first (:output example))))))
            examples)))


(comment

  )
