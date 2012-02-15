(ns str-project-clojure.utils
  (:require [clojure.java.io :as io]))

(defn do-lines
  [path func]
  (with-open [rdr (io/reader path)]
    (doseq [line (line-seq rdr)]
      (func line))))

(defn do-first-line
  [path func]
  (with-open [rdr (io/reader path)]
    (func (first (line-seq rdr)))))

(defn avg-time-sec
  [fn iter]
  (let [start (. java.lang.System nanoTime)]
    (doseq [i (range iter)]
      (fn))
    (/ (double (/ (- (. java.lang.System nanoTime) start)
                  iter))
       1000000000.0)))

(defn substrings
  [^String s]
  (let [l (count s)]
    (for [start (range l)
          end (range (inc start) (inc l))]
      (.substring s start end))))

(defn inversion
  [^String dna]
  (apply str
         (map {\A \C \C \A \G \T \T \G} (reverse dna))))

(defn random-nuc
  []
  (char (+ (rand-int 4) 65)))

(defn overlap-string-blocks
  [s block-len overlap]
  (map #(apply str %) (partition block-len overlap [] s)))