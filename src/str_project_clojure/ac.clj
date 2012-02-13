(ns str-project-clojure.ac
  (:require [clojure.string]
            [clojure.set]))

(defrecord Rule [from to cost])

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
    (if (set? x)
      (set! output (clojure.set/union output x))
      (set! output (conj output x)))
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
     (Node. {} #{} nil false)))

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
  [d x y rule]
  (let [a (- x (count (:from rule)))
        b (- y (count (:to rule)))]
    (+ (let [^"[Lclojure.lang.Delay;" col (aget ^objects d a)]
         (force (aget col b)))
       (:cost rule))))

(defn min-cost
  [d x y rules a-state b-state]
  (reduce min
          Double/POSITIVE_INFINITY
          (map (partial cost d x y)
               (for [i (clojure.set/intersection (output a-state)
                                                 (output b-state))]
                 (rules i)))))

(defn dyn-gen-edit
  [rules]
  (let [a-root (fail-links! (make-ac (map :from rules)))
        b-root (fail-links! (make-ac (map :to rules)))]
    (fn [A B]
      (let [^objects d (make-array clojure.lang.Delay
                                   (inc (count A))
                                   (inc (count B)))]
        (aset d 0 0 (delay (double 0)))
        (loop [x 0 y 1
               a-state (states a-root A)
               b-state (next (cycle (states b-root B)))]
          (cond (> x (count A))
                (force (aget d (count A) (count B)))
                (> y (count B))
                (recur (inc x) 0
                       (next a-state)
                       b-state)
                :else
                (let [^"[Lclojure.lang.Delay;" col (aget d x)]
                  (aset col y
                        (delay
                         (double (min-cost d x y
                                           rules
                                           (first a-state)
                                           (first b-state)))))
                  (recur x (inc y)
                         a-state
                         (next b-state)))))))))