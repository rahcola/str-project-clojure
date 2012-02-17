(ns str-project-clojure.intset
  "Bit vector implementation for a persistent set of integers using
   java.util.BitSet from the standard library. Bit vector offeres fast
   intersection and union operations."
  (:import java.util.BitSet))

(defprotocol AIntSet
  (union [this other])
  (intersection [this other]))

(deftype IntSet [^java.util.BitSet set]
  clojure.lang.Seqable
  (seq [this]
    (loop [is []
           i (.nextSetBit set 0)]
      (if (< i 0)
        (seq is)
        (recur (conj is i)
               (.nextSetBit set (inc i))))))
  clojure.lang.IPersistentCollection
  (count [this]
    (.cardinality set))
  (cons [this o]
    (let [new-set (.clone set)]
      (.set new-set (int o))
      (IntSet. new-set)))
  (empty [this]
    (IntSet. (new java.util.BitSet)))
  (equiv [this other]
    (.equals set (.set other)))
  clojure.lang.IPersistentSet
  (disjoin [this o]
    (let [new-set (.clone set)]
      (.clear new-set (int o))
      (IntSet. new-set)))
  (contains [this o]
    (.get set (int o)))
  (get [this o]
    (.get set (int o)))
  AIntSet
  (union [this other]
    (let [^java.util.BitSet result (.clone set)]
    (.or result (.set other))
    (IntSet. result)))
  (intersection [this other]
    (let [^java.util.BitSet result (.clone set)
          ^java.util.BitSet o (.set other)]
    (.and result o)
    (IntSet. result))))

(defn int-set
  []
  (IntSet. (new java.util.BitSet)))
