(ns king.cbi.algs.jt
  (:require [schema.core :as s]
            [clojure.set :refer [difference]]
            [king.cbi.model.cbi-problem :refer [cbip->clique-tree]]
            [king.cbi.ops :refer :all]
            [plumbing.core :as pc]
            [loom.io :as li]
            [loom.alg :as la]
            [clojure.set :refer [subset?]]

            [taoensso.timbre :refer [spy]]
            )
  (:import [king.cbi.model.variables Variable]))



(defn junction-tree [cbip
                     query-vars
                     elim-ordering-fn]
  (let [tree (cbip->clique-tree cbip (elim-ordering-fn cbip))
        csts (:constraints cbip)
        cliques (:nodeset tree)
        subsets (pc/for-map [c cliques]
                            c
                            (filter (fn [cst] (subset? (:scope cst) c) ) csts))
        initial-clusters  (pc/map-vals #(reduce (combine (:semiring cbip)) %) subsets)
        ]
    ))
