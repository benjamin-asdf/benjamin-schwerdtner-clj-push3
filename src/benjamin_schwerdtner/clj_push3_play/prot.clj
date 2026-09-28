(ns benjamin-schwerdtner.clj-push3-play.prot)

(defprotocol PushValue
  (m-typeof-item [this]))

(extend-protocol PushValue
  java.lang.String
  (m-typeof-item [_this] :push/string)
  java.lang.Boolean
  (m-typeof-item [_this] :push/boolean)
  ;; -------------------
  java.lang.Integer
  (m-typeof-item [_this] :push/integer)
  java.lang.Long
  (m-typeof-item [_this] :push/integer)
  clojure.lang.BigInt
  (m-typeof-item [_this] :push/integer)
  ;; Raw BigInteger is the BIGNUM carrier - the unbounded busy-beaver output
  ;; channel. clojure.lang.BigInt stays an INTEGER (and stays clamped).
  java.math.BigInteger
  (m-typeof-item [_this] :push/bignum)
  ;; ------------------
  java.lang.Float
  (m-typeof-item [_this] :push/float)
  java.lang.Double
  (m-typeof-item [_this] :push/float)
  clojure.lang.Ratio
  (m-typeof-item [_this] :push/float)
  ;; -------------------
  clojure.lang.IPersistentList
  (m-typeof-item [_this] :push/code)
  ;; -------------------
  clojure.lang.Symbol
  (m-typeof-item [_this] :push/name)
  java.lang.Character
  (m-typeof-item [_this] :push/char)
  clojure.lang.Keyword
  (m-typeof-item [_this] :push/clj-object)
  clojure.lang.IPersistentVector
  (m-typeof-item [this] [:push/vector (m-typeof-item (first this))])
  Object
  (m-typeof-item [this]
    (cond
      (:push/type (meta this))
      (:push/type (meta this))

      (= (type this) :pyobject)
      :push/grid

      (list? this)
      :push/code

      :else
      :push/clj-object))

  nil
  (m-typeof-item [_this] :push/clj-object))

(defn with-push-type-meta
  [o push-type]
  (with-meta o (merge (meta o) {:push/type push-type})))

(comment
  (m-typeof-item [1])
  (m-typeof-item '()))
