(ns str-project-clojure.ac
  (:require [clojure.set :as set])
  (:require [str-project-clojure.utils :as utils])
  (:import java.util.BitSet))

(defrecord Rule [from to cost])

(defn make-bitset
  []
  (new java.util.BitSet))

(defn bitset-set
  [^java.util.BitSet a i]
  (.set a i)
  a)

(defn bitset-union
  [^java.util.BitSet a ^java.util.BitSet b]
  (let [^java.util.BitSet result (.clone a)]
    (.or result b)
    result))

(defn bitset-intersection
  [^java.util.BitSet a ^java.util.BitSet b]
  (let [^java.util.BitSet result (.clone a)]
    (.and result b)
    result))

(defn bitset-reduce
  [f acc ^java.util.BitSet set]
  (loop [acc acc
         i (.nextSetBit set 0)]
    (if (< i 0)
      acc
      (recur (f acc i)
             (.nextSetBit set (inc i))))))

(defprotocol ANode
  (push [this sym])
  (children [this])
  (lookup-child [this sym])
  (set-child! [this sym new-child])
  (output [this])
  (add-output! [this x])
  (fail [this])
  (set-fail! [this new-fail])
  (set-root! [this]))

(deftype Node [^:volatile-mutable children
               ^:volatile-mutable output
               ^:volatile-mutable fail
               ^:volatile-mutable root]
  Object
  (toString [this]
    (apply str
           (.hashCode this) "\n"
           (if (nil? fail)
             ""
             (str (.hashCode fail) "\n"))
           (str output "\n")
           (map (fn [[sym child]]
                  (str sym ": " (.hashCode child) "\n"))
                children)))
  ANode
  (push [this sym]
    (loop [state this]
      (if (nil? (lookup-child state sym))
        (recur (fail state))
        (lookup-child state sym))))
  (children [this]
    children)
  (lookup-child [this sym]
    (children sym (if root this)))
  (set-child! [this sym new-child]
    (set! children (assoc children sym new-child))
    this)
  (output [this]
    output)
  (add-output! [this x]
    (if (isa? (class x) java.util.BitSet)
      (.or output x)
      (.set output x))
    this)
  (fail [this]
    fail)
  (set-fail! [this new-fail]
    (set! fail new-fail)
    this)
  (set-root! [this]
    (set-fail! this this)
    (set! root true)
    this))

(defn push-f
  [node sym]
  (loop [state node]
    (if (nil? (lookup-child state sym))
      (recur (fail state))
      (lookup-child state sym))))

(defn make-node
  ([]
     (Node. {} (make-bitset) nil false)))

(defn insert-word!
  [root [i word]]
  (loop [node root
         ws word]
    (if (empty? ws)
      (add-output! node i)
      (let [child (lookup-child node (first ws))]
        (if (nil? child)
          (let [child (make-node)]
            (set-child! node (first ws) child)
            (recur child (next ws)))
          (recur child (next ws))))))
  root)

(defn make-ac
  [words]
  (let [root (make-node)]
    (doseq [word (map-indexed vector words)]
      (insert-word! root word))
    (set-root! root)))

(defn fail-links!
  [root]
  (loop [queue (reduce (fn [queue child]
                         (set-fail! child root)
                         (conj queue child))
                       clojure.lang.PersistentQueue/EMPTY
                       (vals (children root)))]
    (if (empty? queue)
      root
      (recur
       (let [r (peek queue)]
         (loop [queue (pop queue)
                children (children r)]
           (if (empty? children)
             queue
             (let [[a s] (first children)]
               (set-fail! s (loop [state (fail r)]
                              (if (nil? (lookup-child state a))
                                (recur (fail state))
                                (lookup-child state a))))
               (add-output! s (output (fail s)))
               (recur (conj queue s)
                      (next children))))))))))

(defn states
  [root word]
  (reduce (fn [states sym]
            (let [new (push-f (peek states) sym)]
              (conj states new)))
          [root]
          word))

(defn cost
  [^objects d x y rule]
  (let [x (int x)
        y (int y)
        a (- x (.length ^String (:from rule)))
        b (- y (.length ^String (:to rule)))]
    (+ (let [^"[Lclojure.lang.Delay;" col (aget d a)]
         (force (aget col b)))
       (:cost rule))))

(defn collect-matches
  [^objects d]
  (let [w (alength d)
        h (alength (aget d 0))]
    (for [x (range w)
          y (range h)
          :when (and (= y (dec h))
                     (not= (let [^"[Lclojure.lang.Delay;" col (aget d x)]
                             (force (aget col y)))
                           Double/POSITIVE_INFINITY))]
      [(dec x)
       (let [^"[Lclojure.lang.Delay;" col (aget d x)]
         (force (aget col y)))])))

(defn dyn-gen-edit
  [rules matches?]
  (let [rules (vec rules)
        a-root (fail-links! (make-ac (map :from rules)))
        b-root (fail-links! (make-ac (map :to rules)))]
    (fn [^String A ^String B]
      (defn min-cost
        [^objects d x y intersection]
        (bitset-reduce (fn [current-min i]
                         (min current-min
                              (cost d x y (rules i))))
                       (if (and (> x 0) (> y 0)
                                (= (.charAt A (dec x)) (.charAt B (dec y))))
                         (let [^"[Lclojure.lang.Delay;" col (aget d (dec x))]
                           (force (aget col (dec y))))
                         Double/POSITIVE_INFINITY)
                       intersection))
      (let [w (inc (.length A))
            h (inc (.length B))
            ^objects d (make-array clojure.lang.Delay w h)]
        (loop [x 0 y 0
               a-state a-root
               b-state b-root]
          (let [^"[Lclojure.lang.Delay;" col (aget d x)]
            (aset col y
                  (delay
                   (if (and (= y 0) (or matches? (= x 0)))
                     0
                     (min-cost d x y
                               (bitset-intersection (output a-state)
                                                    (output b-state)))))))
          (cond (and (= x (dec w))
                     (= y (dec h)))
                (if matches?
                  (collect-matches d)
                  (force (aget d (.length A) (.length B))))
                (= y (dec h))
                (recur (inc x)
                       0
                       (push-f a-state (.charAt A x))
                       b-root)
                :else
                (recur x
                       (inc y)
                       a-state
                       (push-f b-state (.charAt B y)))))))))