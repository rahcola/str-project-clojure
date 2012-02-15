(ns str-project-clojure.test.performance
  (:require [str-project-clojure.basic :as basic])
  (:require [str-project-clojure.ac :as ac])
  (:require [str-project-clojure.utils :as utils])
  (:use criterium.core)
  (:use [clojure.string :only [join]]))

(let [a (join "" (repeat 100 "a"))
      b (join "" (repeat 100 "b"))
      rules [{:from "a" :to "b" :cost 1}]
      g (ac/dyn-gen-edit rules false)]
  (quick-bench (basic/dyn-gen-edit rules a b false))
  (quick-bench (g a b)))

(let [a (join "" (repeat 100 "a"))
      b (join "" (repeat 100 "b"))
      rules [{:from "c" :to "b" :cost 1}]
      g (ac/dyn-gen-edit rules false)]
  (quick-bench (basic/dyn-gen-edit rules a b false))
  (quick-bench (g a b)))

(let [a (join "" (repeat 1000 "a"))
      b (join "" (repeat 1000 "b"))
      rules [{:from (join "" (repeat 10 "a"))
              :to (join "" (repeat 10 "b"))
              :cost 1}]
      g (ac/dyn-gen-edit rules false)]
  (quick-bench (basic/dyn-gen-edit rules a b false))
  (quick-bench (g a b)))

(let [a (join "" (repeat 1000 "a"))
      b (join "" (repeat 1000 "b"))
      rules [{:from (join "" (repeat 10 "c"))
              :to (join "" (repeat 10 "b"))
              :cost 1}]
      g (ac/dyn-gen-edit rules false)]
  (quick-bench (basic/dyn-gen-edit rules a b false))
  (quick-bench (g a b)))

(let [pattern "TCGTTCAATAAAAGTCCTCAAGAGGTTGGTTAATACGCATGTTTAATAG"
      rules (map (fn [f t c] {:from f :to t :cost c})
                 (utils/substrings pattern)
                 (map utils/inversion (utils/substrings pattern))
                 (repeat 1))
      block-len (* (count pattern) 2)
      overlap (count pattern)]
  (utils/do-first-line
   "resources/dna.50MB"
   (fn [line]
     (let [text (utils/overlap-string-blocks line block-len overlap)
           g (ac/dyn-gen-edit rules true)]
       (println "Naive:")
       (quick-bench (last (map #(basic/dyn-gen-edit rules % pattern true)
                               (take 100 text))))
       (println "AC:")
       (quick-bench (last (map #(g % pattern)
                               (take 100 text))))))))