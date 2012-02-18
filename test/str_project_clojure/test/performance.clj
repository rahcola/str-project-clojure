(ns str-project-clojure.test.performance
  "Benchmarks for general edit distance calculation. BEWARE, these
   take a long time to run."
  (:require [str-project-clojure.basic :as basic])
  (:require [str-project-clojure.ac :as ac])
  (:require [str-project-clojure.utils :as utils])
  (:use criterium.core)
  (:require [clojure.string :as string]))

(defn mean-bench
  "Benchmark f using criterium, and return the mean."
  [f]
  (first (:mean (benchmark (f) :reduce-with (constantly nil)))))

(defn csv-string
  [results]
  (string/join "\n"
               (map (fn [result]
                      (str (first result) \tab (second result)))
                    results)))

(def ac-dna-inversions
  "Using 1000 first symbols from the first line of resources/dna.50MB
   as a text, try to find a pattern of random dna string by allowing
   any substring of the pattern to be inversed. Return the time taken
   by the calculation for patterns of length 5, 10, 15, ..., 195."
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

(spit "resources/ac_dna_inversions.csv" (csv-string ac-dna-inversions))

(def basic-dna-inversions
  "Same as ac-dna-inversions, but using the basic algorithm."
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

(spit "resources/basic_dna_inversions" (csv-string basic-dna-inversions))

(def basic-long-rules
  (let [a (string/join "" (repeat 200 \a))
        b (string/join "" (repeat 200 \b))]
    (map (fn [l]
           (let [rules [{:from (string/join "" (repeat l \a))
                         :to (string/join "" (repeat l \b))
                         :cost 1}
                        {:from (string/join "" (repeat (/ l 2) \a))
                         :to (string/join "" (repeat (/ l 2) \a))
                         :cost 2}]
                 basic (basic/dyn-gen-edit rules)]
             [l (mean-bench (fn [] (basic a b)))]))
         (range 5 200 5))))

(spit "resources/basic_long_rules.csv" (csv-string basic-long-rules))

(def ac-long-rules
  (let [a (string/join "" (repeat 200 \a))
        b (string/join "" (repeat 200 \b))]
    (map (fn [l]
           (let [rules [{:from (string/join "" (repeat l \a))
                         :to (string/join "" (repeat l \b))
                         :cost 1}
                        {:from (string/join "" (repeat (/ l 2) \a))
                         :to (string/join "" (repeat (/ l 2) \a))
                         :cost 2}]
                 ac (ac/dyn-gen-edit rules)]
             [l (mean-bench (fn [] (ac a b)))]))
         (range 5 200 5))))

(spit "resources/ac_long_rules.csv" (csv-string ac-long-rules))

(def basic-devil-rules
  (let [a (string/join "" (repeat 200 \a))
        b (string/join "" (repeat 200 \b))]
    (map (fn [l]
           (let [rules (map (fn [l]
                              {:from (string/join "" (repeat l \a))
                               :to (string/join "" (cons \c (repeat l \b)))
                               :cost 1})
                            (range 1 l))
                 basic (basic/dyn-gen-edit rules)]
             [l (mean-bench (fn [] (basic a b)))]))
         (range 5 200 5))))

(spit "resources/basic_devil_rules.csv" (csv-string basic-devil-rules))

(def ac-devil-rules
  (let [a (string/join "" (repeat 200 \a))
        b (string/join "" (repeat 200 \b))]
    (map (fn [l]
           (let [rules (map (fn [l]
                              {:from (string/join "" (repeat l \a))
                               :to (string/join "" (cons \c (repeat l \b)))
                               :cost 1})
                            (range 1 l))
                 ac (ac/dyn-gen-edit rules)]
             [l (mean-bench (fn [] (ac a b)))]))
         (range 5 200 5))))

(spit "resources/ac_devil_rules.csv" (csv-string ac-devil-rules))