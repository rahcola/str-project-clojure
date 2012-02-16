(ns str-project-clojure.basic
  (:require [str-project-clojure.utils :as utils]))

(defrecord Rule [from to cost])

(defn dyn-gen-edit
  [rules & {:keys [full-match]
            :or {full-match true}}]
  (fn [^String A ^String B]
    (defn cost
      [^objects d x y rule]
      (if (and (.endsWith ^String (subs A 0 x) ^String (:from rule))
               (.endsWith ^String (subs B 0 y) ^String (:to rule)))
        (let [x (int x)
              y (int y)
              a (- x (.length ^String (:from rule)))
              b (- y (.length ^String (:to rule)))]
          (+ (force (utils/d-get d a b))
             (:cost rule)))
        Double/POSITIVE_INFINITY))
    (defn min-cost
      [^objects d x y]
      (reduce min
              (if (and (> x 0) (> y 0)
                       (= (.charAt A (dec x)) (.charAt B (dec y))))
                (force (utils/d-get d (dec x) (dec y)))
                Double/POSITIVE_INFINITY)
              (map (partial cost d x y) rules)))
    (let [^objects d (make-array clojure.lang.Delay
                                 (inc (count A))
                                 (inc (count B)))]
      (doseq [x (range (inc (count A)))
              y (range (inc (count B)))]
        (utils/d-set d x y
                     (delay
                      (if (and (= y 0) (or (not full-match) (= x 0)))
                        0
                        (min-cost d x y)))))
      (if full-match
        (force (utils/d-get d (count A) (count B)))
        (utils/collect-matches d)))))