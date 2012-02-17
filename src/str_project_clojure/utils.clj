(ns str-project-clojure.utils
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string]))

(defn do-lines
  "Call func for every line of the file in path."
  [path func]
  (with-open [rdr (io/reader path)]
    (doseq [line (line-seq rdr)]
      (func line))))

(defn do-first-line
  "Call func for the first line of the file in path."
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
  "Return contiguous subsequences of s."
  [^String s]
  (let [l (count s)]
    (for [start (range l)
          end (range (inc start) (inc l))]
      (.substring s start end))))

(defn inversion
  "Return the inversion of dna string."
  [^String dna]
  (apply str
         (map {\A \C \C \A \G \T \T \G} (reverse dna))))

(defn random-nuc
  []
  ({0 \A
    1 \C
    2 \T
    3 \G} (rand-int 4)))

(defn random-dna-string
  [l]
  (string/join "" (for [i (range l)] (random-nuc))))

(defn overlaping-string-blocks
  [s block-len overlap]
  (map #(apply str %) (partition block-len overlap [] s)))

(defn d-get [^objects d x y]
  (let [^"[Lclojure.lang.Delay;" col (aget d x)]
    (aget col y)))

(defn d-set [^objects d x y val]
  (let [^"[Lclojure.lang.Delay;" col (aget d x)]
    (aset col y val)))

(defn collect-matches
  [^objects d]
  (let [w (alength d)
        h (alength (aget d 0))]
    (for [x (range w)
          y (range h)
          :when (and (= y (dec h))
                     (not= (force (d-get d x y))
                           Double/POSITIVE_INFINITY))]
      [(dec x) (force (d-get d x y))])))
