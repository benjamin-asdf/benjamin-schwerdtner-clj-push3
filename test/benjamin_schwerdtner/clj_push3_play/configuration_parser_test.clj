(ns benjamin-schwerdtner.clj-push3-play.configuration-parser-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [benjamin-schwerdtner.clj-push3-play.configuration.parser :as parser]
   [benjamin-schwerdtner.clj-push3-play.instructions.interface :as instructions]))

(def example
  "# a comment
  # an indented comment

MAX-RANDOM-FLOAT 1.0
EVALPUSH-LIMIT 1000
TOP-LEVEL-POP-CODE FALSE
type FLOAT
type integer
instruction INTEGER.FROMBOOLEAN
instruction INTEGER.>")

(deftest read-configuration-test
  (is (= {:parameters {:max-random-float 1.0
                       :evalpush-limit 1000
                       :top-level-pop-code false}
          :types [:push/float :push/integer]
          :instructions [:integer_fromboolean :integer_>]}
         (parser/read-configuration example))))

(deftest instruction-names-resolve-test
  (testing "parsed instruction names are keys of the instruction table"
    (let [known (set (instructions/all-instructions))]
      (is (every? known (:instructions (parser/read-configuration example)))))))

(deftest malformed-input-test
  (is (thrown? clojure.lang.ExceptionInfo
               (parser/read-configuration "type SPACESHIP")))
  (is (thrown? clojure.lang.ExceptionInfo
               (parser/read-configuration "EVALPUSH-LIMIT")))
  (is (thrown? clojure.lang.ExceptionInfo
               (parser/read-configuration "EVALPUSH-LIMIT 1 2"))))
