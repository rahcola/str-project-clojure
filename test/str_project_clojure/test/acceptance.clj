(ns str-project-clojure.test.acceptance
  (:require [str-project-clojure.ac :as ac])
  (:require [str-project-clojure.basic :as basic])
  (:use midje.sweet))

(fact
 (let [words ["she" "he" "his" "hers"]
       root (ac/fail-links! (ac/make-ac words))]
   (ac/output (reduce ac/push-f root "he")) => #{1}
   (ac/output (reduce ac/push-f root "hers")) => #{3}
   (ac/output (reduce ac/push-f root "his")) => #{2}
   (ac/output (reduce ac/push-f root "she")) => #{0 1}))

(fact
 (let [a "abcabc"
       b "cdcd"
       rules (map #(zipmap [:from :to :cost] %)
                  [["abc" "cd" 2]
                   ["ab" "c" 1]
                   ["ca" "d" 1]
                   ["bc" "cd" 1]])]
   ((ac/dyn-gen-edit rules false) a b) => (double 3)
   (basic/dyn-gen-edit rules a b) => (double 3)))

(fact
 (let [a "abcabc"
       b "cd"
       rules (map #(zipmap [:from :to :cost] %)
                  [["abc" "cd" 2]
                   ["ab" "c" 1]
                   ["ca" "d" 1]
                   ["bc" "cd" 1]])]
   ((ac/dyn-gen-edit rules true) a b) => (list [2 (double 1)]
                                               [3 (double 2)]
                                               [5 (double 1)])
   (basic/dyn-gen-edit rules a b) => Double/POSITIVE_INFINITY))

(fact
 (let [a "helmi"
       b "kuppi"
       rules (map #(zipmap [:from :to :cost] %)
                  [["h" "k" 1]
                   ["e" "u" 1]
                   ["l" "p" 1]
                   ["m" "p" 1]])]
   ((ac/dyn-gen-edit rules false) a b) => (double 4)
   (basic/dyn-gen-edit rules a b) => (double 4)))

(fact
 (let [a "helmi"
       b "kuppi"
       rules (map #(zipmap [:from :to :cost] %)
                  [["h" "k" 1]
                   ["e" "u" 1]
                   ["l" "p" 1]
                   ["m" "p" 1]
                   ["i" "i" -1]])]
   ((ac/dyn-gen-edit rules false) a b) => (double 3)
   (basic/dyn-gen-edit rules a b) => (double 3)))