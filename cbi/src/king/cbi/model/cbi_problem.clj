(ns king.cbi.model.cbi-problem
  (:require [schema.core :as s]
            [king.semirings :as sr]
            [king.cbi.model.variables ]
            [king.cbi.model.constraints]
            [king.graph :as kg]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.set :refer [union subset?]]
            [taoensso.timbre :refer [spy]]
            [loom.graph :as lg]
            [plumbing.core :as pc]

            )

  (:import [king.semirings Semiring]
           [king.cbi.model.variables Variable]
           [king.cbi.model.constraints Constraint])
  )

(s/defrecord CBIProblem [scope :- #{Variable}
                         semiring :- Semiring
                         constraints :- #{Constraint}])

(s/defn cbi-problem [scope :- #{Variable}
                     semiring :- Semiring
                     constraints :- #{Constraint}]
  (assert (= scope (apply union (map :scope constraints))))
  (->CBIProblem scope semiring constraints))


(s/defn plain-order [cbip]
  (:scope cbip)
  )

(s/defn rand-order [cbip]
  (shuffle (:scope cbip))
  )

(defn cbip->primal-graph [cbip]
  (let [edge-sets (for [scope (map :scope (:constraints cbip))]
                    (combinations scope 2))
        edges (reduce concat [] edge-sets)]
    (reduce
     (fn add-edge [g e]
       (lg/add-edges g e))
     (lg/graph)
     edges)
    ))

(defn cbip->clique-tree [cbip ordering]
  (kg/clique-tree (spy (cbip->primal-graph cbip)) ordering))

(defn cbip->factor-graph [cbip]
  (let [adj (pc/map-from-keys :scope (:constraints cbip) )]
    (lg/graph adj)
    )

  )
(defn nuisances [cbip query-vars]
  (filter (fn [v] (not (contains? query-vars v)))
          (:scope cbip)))
