#!/bin/sh

java -cp `cat classpath` clojure.main test/str_project_clojure/test/performance.clj
