(ns str-project-clojure.basic)

(defrecord Rule [from to cost])

(defn lazy-gen-edit
  [rules A B]
  (let [f (fn [^String A ^String B d rule]
            (if (and (.endsWith A (:from rule))
                     (.endsWith B (:to rule)))
              (let [a (- (count A) (count (:from rule)))
                    b (- (count B) (count (:to rule)))]
                (+ (force (nth (nth d a) b))
                   (:cost rule)))
              Double/POSITIVE_INFINITY))]
    (loop [d [[0]]
           x 0 y 1]
      (if (and (> x (count A)))
        (force (peek (peek d)))
        (let [val (delay
                   (reduce min
                           0
                           (map (partial f (subs A 0 x) (subs B 0 y) d)
                                rules)))]
          (recur (if (= y 0)
                   (conj d [val])
                   (conj (pop d)
                         (conj (peek d) val)))
                 (if (= y (count B)) (inc x) x)
                 (if (= y (count B)) 0 (inc y))))))))

(defn dyn-gen-edit
  [rules A B]
  (let [^objects d (make-array Double/TYPE
                               (inc (count A))
                               (inc (count B)))
        f (fn [^String A ^String B rule]
            (if (and (.endsWith A (:from rule))
                     (.endsWith B (:to rule)))
              (let [a (- (count A) (count (:from rule)))
                    b (- (count B) (count (:to rule)))]
                (+ (let [^doubles col (aget d a)]
                     (aget col b))
                   (:cost rule)))
              Double/POSITIVE_INFINITY))]
    (doseq [x (range (inc (count A)))
            y (range (inc (count B)))]
      (if (= x y 0)
        (aset d x y (double 0))
        (let [^doubles col (aget d x)]
          (aset col y
                (double
                 (reduce min
                         Double/POSITIVE_INFINITY
                         (map (partial f (subs A 0 x) (subs B 0 y))
                              rules)))))))
    (aget d (count A) (count B))))

(def A (clojure.string/join "" (repeat 1000 "abc")))
(def B (clojure.string/join "" (repeat 1000 "cd")))

(def t [(Rule. "abc" "cd" 2)
        (Rule. "ab" "c" 1)
        (Rule. "ca" "d" 1)
        (Rule. "bc" "cd" 1)])