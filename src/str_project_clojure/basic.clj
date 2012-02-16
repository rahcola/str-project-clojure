(ns str-project-clojure.basic)

(defrecord Rule [from to cost])

(defn dyn-gen-edit
  [rules ^String A ^String B matches?]
  (defn cost
    [^objects d x y rule]
    (if (and (.endsWith ^String (subs A 0 x) ^String (:from rule))
             (.endsWith ^String (subs B 0 y) ^String (:to rule)))
      (let [x (int x)
            y (int y)
            a (- x (.length ^String (:from rule)))
            b (- y (.length ^String (:to rule)))]
        (+ (let [^"[Lclojure.lang.Delay;" col (aget d a)]
             (force (aget col b)))
           (:cost rule)))
      Double/POSITIVE_INFINITY))
  (defn min-cost
    [^objects d x y]
    (reduce min
            (if (and (> x 0) (> y 0)
                     (= (.charAt A (dec x)) (.charAt B (dec y))))
              (let [^"[Lclojure.lang.Delay;" col (aget d (dec x))]
                (force (aget col (dec y))))
              Double/POSITIVE_INFINITY)
            (map (partial cost d x y) rules)))
  (let [^objects d (make-array clojure.lang.Delay
                               (inc (count A))
                               (inc (count B)))]
    (doseq [x (range (inc (count A)))
            y (range (inc (count B)))]
      (let [^"[Lclojure.lang.Delay;" col (aget d x)]
        (aset col y
              (delay
               (if (and (= y 0) (or matches? (= x 0)))
                 0
                 (min-cost d x y))))))
    (if matches?
      (for [x (range (inc (count A)))
            y (range (inc (count B)))
            :when (and (= y (count B))
                       (not= (force (aget d x y))
                             Double/POSITIVE_INFINITY))]
        [(dec x) (force (aget d x y))])
      (force (aget d (count A) (count B))))))