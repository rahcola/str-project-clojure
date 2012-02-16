(ns str-project-clojure.test.performance
  (:require [str-project-clojure.basic :as basic])
  (:require [str-project-clojure.ac :as ac])
  (:require [str-project-clojure.utils :as utils])
  (:use criterium.core)
  (:require [clojure.string :as string]))

(defn mean-bench
  [f]
  (first (:mean (benchmark (f) :reduce-with (constantly nil)))))

(defn csv-string
  [results]
  (string/join "\n"
               (map (fn [result]
                      (str (first result) \tab (second result)))
                    results)))

(def ac-dna-inversions
  (utils/do-first-line
   "resources/dna.50MB"
   (fn [line]
     (map (fn [l]
            (let [text (string/join "" (take 1000 line))
                  pattern (utils/random-dna-string l)
                  rules (map (fn [f t c] {:from f :to t :cost c})
                             (utils/substrings pattern)
                             (map utils/inversion (utils/substrings pattern))
                             (repeat 1))
                  ac (ac/dyn-gen-edit rules :full-match false)]
              [l
               (mean-bench (fn [] (ac text pattern)))]))
          (range 5 200 5)))))

(def basic-dna-inversions
  (utils/do-first-line
   "resources/dna.50MB"
   (fn [line]
     (map (fn [l]
            (let [text (string/join "" (take 1000 line))
                  pattern (utils/random-dna-string l)
                  rules (map (fn [f t c] {:from f :to t :cost c})
                             (utils/substrings pattern)
                             (map utils/inversion (utils/substrings pattern))
                             (repeat 1))
                  basic (basic/dyn-gen-edit rules :full-match false)]
              [l
               (mean-bench (fn [] (basic text pattern)))]))
          (range 5 200 5)))))