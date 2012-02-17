(ns str-project-clojure.ac
  "General edit distance calculation using an Aho-Corasick automaton."
  (:require [str-project-clojure.utils :as utils])
  (:require [str-project-clojure.intset :as intset]))

(defprotocol ANode
  "Node of the Aho-Corasick Automaton. Methods with ! mutate the ANode
   in place for efficiency. The children of the node are represented
   as a hash map to allow arbitary symbols in a reasonable time and
   space. The current clojure implementation guarantees O(log_32(n))
   lookup. The output set is represented by a bit vector. See
   intset.clj."
  (children [this] "Return a map from symbols to child nodes.")
  (lookup-child [this sym] "Return the child reachable with symbol
  sym. Nil if no such child, or this if this is a root node.")
  (set-child! [this sym new-child] "Set new-child to be reachable with sym.")
  (output [this] "IntSet of indexes to words. See make-ac.")
  (add-output! [this x] "If x is an IntSet, set output to (union
  output x). Else add x to output.")
  (fail [this] "Link to a node representing the longest proper suffix
  of the string represented by this node.")
  (set-fail! [this new-fail] "Set the fail link.")
  (set-root! [this] "Set this node to be a root node. See lookup-child."))

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
    (if (satisfies? intset/AIntSet x)
      (set! output (intset/union output x))
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
  "Return the child reachable with sym. If no such child, follow fail
  links until such child is found."

  [node sym]
  (loop [state node]
    (if (nil? (lookup-child state sym))
      (recur (fail state))
      (lookup-child state sym))))

(defn make-node
  ([]
     (Node. {} (intset/int-set) nil false)))

(defn insert-word!
  "Insert a word in to a AC automaton and return root. Walk down the
  automaton using lookup-child, and create nodes if required."

  [root
   [i word]]
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
  "Return a root of an AC automaton, NOT ready for use. See
  fail-links!. Output of a node is a set of indexes to the sequence
  words."

  [words]
  (let [root (make-node)]
    (doseq [word (map-indexed vector words)]
      (insert-word! root word))
    (set-root! root)))

(defn fail-links!
  "Set the fail links in an AC automaton, and return root. This also
  fills the output sets and stores them in the nodes."

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

(defn cost
  "Cost of turning string A[:x] to B[:y] by modifying the end of A[:x]
  as permited by rule plus the cost of turning A[:a] to B[:b]"
  [^objects d x y rule]
  (let [x (int x)
        y (int y)
        a (- x (.length ^String (:from rule)))
        b (- y (.length ^String (:to rule)))]
    (+ (force (utils/d-get d a b))
       (:cost rule))))

(defn dyn-gen-edit
  "Return a function that calculates the general edit distance between
  A and B using the rules given. Rules are of form {:from string :to
  string :cost double}. If :full-match false, calculate the edit
  distance between any substring of A and B, and return every match
  and its cost as a sequence of [i c] pairs, where i is the position
  of the end of the match in A and c is the cost. A dynamic
  programming method of tabulation is used, but the matrix is filled
  with promises of computation, created by delay. The promises are
  forced by the cost function only when needed."

  [rules & {:keys [full-match]
            :or {full-match true}}]
  (let [rules (object-array rules) ;to speed up lookup
        a-root (fail-links! (make-ac (map :from rules)))
        b-root (fail-links! (make-ac (map :to rules)))]
    (fn [^String A ^String B]
      (defn min-cost
        [^objects d x y intersection]
        (reduce (fn [current-min i]
                  (min current-min
                       (cost d x y (aget ^objects rules i))))
                (if (and (> x 0) (> y 0)
                         (= (.charAt A (dec x)) (.charAt B (dec y))))
                  (force (utils/d-get d (dec x) (dec y)))
                  Double/POSITIVE_INFINITY)
                intersection))
      (let [w (inc (.length A))
            h (inc (.length B))
            ^objects d (make-array clojure.lang.Delay w h)]
        (loop [x 0 y 0
               a-state a-root
               b-state b-root]
          (utils/d-set d x y
                       (delay
                        (if (and (= y 0) (or (not full-match) (= x 0)))
                          0
                          (min-cost d x y
                                    (intset/intersection (output a-state)
                                                         (output b-state))))))
          (cond (and (= x (dec w))
                     (= y (dec h)))
                (if full-match
                  (force (utils/d-get d (.length A) (.length B)))
                  (utils/collect-matches d))
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
