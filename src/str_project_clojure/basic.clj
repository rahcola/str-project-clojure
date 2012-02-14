(ns str-project-clojure.basic)

(defrecord Rule [from to cost])

(defn cost
  [^objects d ^String A ^String B rule]
  (if (and (.endsWith A (:from rule))
           (.endsWith B (:to rule)))
    (let [a (- (count A) (count (:from rule)))
          b (- (count B) (count (:to rule)))]
      (+ (let [^"[Lclojure.lang.Delay;" col (aget d a)]
           (force (aget col b)))
         (:cost rule)))
    Double/POSITIVE_INFINITY))

(defn min-cost
  [d ^String A ^String B rules]
  (reduce min
          (if (= (last A) (last B))
            (let [^"[Lclojure.lang.Delay;" col (aget d (dec (count A)))]
              (force (aget col (dec (count B)))))
            Double/POSITIVE_INFINITY)
          (map (partial cost d A B) rules)))

(defn dyn-gen-edit
  [rules A B]
  (let [^objects d (make-array clojure.lang.Delay
                               (inc (count A))
                               (inc (count B)))]
    (doseq [x (range (inc (count A)))
            y (range (inc (count B)))]
      (if (= x y 0)
        (aset d x y (delay (double 0)))
        (let [^"[Lclojure.lang.Delay;" col (aget d x)]
          (aset col y
                (delay
                 (double (min-cost d (subs A 0 x) (subs B 0 y) rules)))))))
    (force (aget d (count A) (count B)))))