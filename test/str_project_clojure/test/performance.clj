(ns str-project-clojure.test.performance
  (:require [str-project-clojure.basic :as basic])
  (:use criterium.core)
  (:use [clojure.string :only [join]]))

(let [a (join "" (repeat 100 "a"))
      b (join "" (repeat 100 "b"))
      rules [{:from "a" :to "b" :cost 1}]]
  (bench (basic/dyn-gen-edit rules a b) :verbose))