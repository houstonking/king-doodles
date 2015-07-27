(ns king.graph-test
  (:require [clojure.test :refer :all]
            [king.graph :refer :all]
            [loom.io :as li]
            [loom.graph :as lg]
            [loom.alg :as la]
            ))

(def student-problem-graph
  (lg/digraph {:C [:D]
               :D [:G]
               :I [:G :S]
               :G [:H :L]
               :S [:J]
               :L [:J]
               :J [:H]}))

(def fish-graph
  (lg/digraph {:A [:C]
               :B [:C]
               :C [:D :E]
               :D [:F]
               :E [:F]}))

(def tank-graph
  (lg/digraph {:A [:B :C]
               :B [:D :F]
               :C [:E]
               :E [:F]})
  )
