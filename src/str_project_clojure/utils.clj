(ns str-project-clojure.utils
  (:require [clojure.java.io :as io]))

(defn do-lines
  [path func]
  (with-open [rdr (io/reader path)]
    (doseq [line (line-seq rdr)]
      (func line))))

(defn avg-time-sec
  [fn iter]
  (let [start (. java.lang.System nanoTime)]
    (doseq [i (range iter)]
      (fn))
    (/ (double (/ (- (. java.lang.System nanoTime) start)
                  iter))
       1000000000.0)))