(ns str-project-clojure.utils)

(defn avg-time-sec
  [fn iter]
  (let [start (. java.lang.System nanoTime)]
    (doseq [i (range iter)]
      (fn))
    (/ (double (/ (- (. java.lang.System nanoTime) start)
                  iter))
       1000000.0)))