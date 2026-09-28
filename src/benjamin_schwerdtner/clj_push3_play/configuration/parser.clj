(ns benjamin-schwerdtner.clj-push3-play.configuration.parser
  "Reader for Push3 configuration files.

  A Push3 configuration file is a plain text file. Any line beginning with
  \"#\" is a comment and is ignored. Actual configuration lines come in three
  forms:

    <parameter name> <value>
    type <type name>
    instruction <instruction name>

  `read-configuration` returns the same shape as `config/defaults`:
  `{:parameters {...} :types [...] :instructions [...]}`, with parameter names
  as kebab-case keywords (MAX-RANDOM-FLOAT -> :max-random-float), types as
  stack keywords (FLOAT -> :push/float) and instructions as instruction-table
  keywords (INTEGER.FROMBOOLEAN -> :integer_fromboolean)."
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]))

(def read-type
  {"FLOAT" :push/float
   "INTEGER" :push/integer
   "BOOLEAN" :push/boolean
   "STRING" :push/string
   "CHAR" :push/char
   "CODE" :push/code
   "EXEC" :push/exec
   "NAME" :push/name})

(defn read-instruction
  "INTEGER.FROMBOOLEAN -> :integer_fromboolean"
  [s]
  (keyword (str/replace (str/lower-case s) "." "_")))

(defn read-value
  "Parameter values are numbers or TRUE/FALSE. Anything else stays a string."
  [s]
  (case (str/upper-case s)
    "TRUE" true
    "FALSE" false
    (let [v (try (edn/read-string s) (catch Exception _ nil))]
      (if (number? v) v s))))

(defn- blank-or-comment? [line]
  (or (str/blank? line) (re-find #"^\s*#" line)))

(defn read-lines
  "Lines -> [key value] pairs, one per configuration line."
  [lines]
  (sequence
   (comp (remove blank-or-comment?)
         (map str/trim)
         (map (fn [line]
                (let [[k v & more] (str/split line #"\s+")]
                  (when (or (nil? v) (seq more))
                    (throw (ex-info "Malformed Push3 configuration line"
                                    {:line line})))
                  [(keyword (str/lower-case k)) v]))))
   lines))

(defn read-configuration
  "Parse the text of a Push3 configuration file."
  [s]
  (reduce
   (fn [acc [k v]]
     (case k
       :type (update acc :types conj
                     (or (read-type (str/upper-case v))
                         (throw (ex-info "Unknown Push3 type" {:type v}))))
       :instruction (update acc :instructions conj (read-instruction v))
       (assoc-in acc [:parameters k] (read-value v))))
   {:parameters {} :types [] :instructions []}
   (read-lines (str/split-lines s))))

(defn read-configuration-file [path]
  (read-configuration (slurp path)))

(comment
  (read-configuration
   "# foo
    MAX-RANDOM-FLOAT 1.0
    EVALPUSH-LIMIT 1000
    TOP-LEVEL-POP-CODE FALSE
    type FLOAT
    instruction INTEGER.FROMBOOLEAN
    instruction INTEGER.>")
  ;; {:parameters {:max-random-float 1.0
  ;;               :evalpush-limit 1000
  ;;               :top-level-pop-code false}
  ;;  :types [:push/float]
  ;;  :instructions [:integer_fromboolean :integer_>]}
  )
