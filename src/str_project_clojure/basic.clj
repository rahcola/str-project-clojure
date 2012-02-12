(ns str-project-clojure.basic)

(defrecord Rule [from to cost])

(defn lazy-gen-edit
  [rules A B]
  (let [f (fn [A B d rule]
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
                   (apply min
                          (map (partial f (subs A 0 x) (subs B 0 y) d)
                               rules)))]
          (recur (if (= y 0)
                   (conj d [val])
                   (conj (pop d)
                         (conj (peek d) val)))
                 (if (= y (count B)) (inc x) x)
                 (if (= y (count B)) 0 (inc y))))))))

(def A (clojure.string/join "" (repeat 100 "abc")))
(def B (clojure.string/join "" (repeat 100 "cd")))

(def t [(Rule. "abc" "cd" 2)
        (Rule. "ab" "c" 1)
        (Rule. "ca" "d" 1)
        (Rule. "bc" "cd" 1)])