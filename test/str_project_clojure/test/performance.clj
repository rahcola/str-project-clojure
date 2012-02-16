(ns str-project-clojure.test.performance
  (:require [str-project-clojure.basic :as basic])
  (:require [str-project-clojure.ac :as ac])
  (:require [str-project-clojure.utils :as utils])
  (:use criterium.core)
  (:use [clojure.string :only [join]]))

(defn mean-bench
  [f]
  (first (:mean (quick-benchmark (f) :reduce-with (constantly nil)))))

(defn csv-string
  [results]
  (join "\n"
        (map (fn [result]
               (str (first result) \tab (second result)))
             results)))

(def ac-dna-inversions
  (map (fn [l]
         (let [pattern (utils/random-dna-string l)
               rules (map #({:from %1 :to %2 :cost %3})
                          (utils/substrings pattern)
                          (map utils/inversion (utils/substrings pattern))
                          (repeat 1))
               ac (ac/dyn-gen-edit rules :full-match false)])
         [l
          (mean-bench (fn [] (ac text pattern)))])
       (range 10 100 10)))