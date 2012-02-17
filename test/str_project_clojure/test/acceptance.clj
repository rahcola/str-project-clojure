(ns str-project-clojure.test.acceptance
  (:require [str-project-clojure.ac :as ac])
  (:require [str-project-clojure.basic :as basic])
  (:require [str-project-clojure.utils :as utils])
  (:require [str-project-clojure.intset :as intset])
  (:use midje.sweet))

(fact
 (let [words ["she" "he" "his" "hers"]
       root (ac/fail-links! (ac/make-ac words))]
   (.set (ac/output (reduce ac/push-f root "he"))) => (.set (conj (intset/int-set) 1))
   (.set (ac/output (reduce ac/push-f root "hers"))) => (.set (conj (intset/int-set) 3))
   (.set (ac/output (reduce ac/push-f root "his"))) => (.set (conj (intset/int-set) 2))
   (.set (ac/output (reduce ac/push-f root "she"))) => (.set (-> (intset/int-set)
                                                    (conj 0)
                                                    (conj 1)))))

(facts "basic sanity check"
 (let [a "abcabc"
       b "cdcd"
       rules (map #(zipmap [:from :to :cost] %)
                  [["abc" "cd" 2]
                   ["ab" "c" 1]
                   ["ca" "d" 1]
                   ["bc" "cd" 1]])]
   ((ac/dyn-gen-edit rules) a b) => 3
   ((basic/dyn-gen-edit rules) a b) => 3))

(facts "setting :full-match to false should give a sequence of pairs
       [a b] where a is the position of the end of the match in a and
       b is the cost"
 (let [a "abcabc"
       b "cd"
       rules (map #(zipmap [:from :to :cost] %)
                  [["abc" "cd" 2]
                   ["ab" "c" 1]
                   ["ca" "d" 1]
                   ["bc" "cd" 1]])
       result (list [2  1]
                    [3  2]
                    [5  1])]
   ((ac/dyn-gen-edit rules :full-match false) a b) => result
   ((basic/dyn-gen-edit rules :full-match false) a b) => result))

(facts "There should be no cost for symbols that already match"
 (let [a "helmi"
       b "kuppi"
       rules (map #(zipmap [:from :to :cost] %)
                  [["h" "k" 1]
                   ["e" "u" 1]
                   ["l" "p" 1]
                   ["m" "p" 1]])]
   ((ac/dyn-gen-edit rules) a b) => 4
   ((basic/dyn-gen-edit rules) a b) => 4))

(facts "test for negative costs"
 (let [a "helmi"
       b "kuppi"
       rules (map #(zipmap [:from :to :cost] %)
                  [["h" "k" 1]
                   ["e" "u" 1]
                   ["l" "p" 1]
                   ["m" "p" 1]
                   ["i" "i" -1]])]
   ((ac/dyn-gen-edit rules) a b) => 3
   ((basic/dyn-gen-edit rules) a b) => 3))
