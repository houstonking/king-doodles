(ns king.cbi.model.cbi-problem
  (:require [schema.core :as s]
            [king.semirings :as sr]
            [king.cbi.model.variables]
            [king.cbi.model.constraints :refer [apply-evidence
                                                calculate]]
            [clojure.set :refer [union subset?]]
            [taoensso.timbre :refer [spy]])

  (:import [king.semirings Semiring]
           [king.cbi.model.variables Variable]
           [king.cbi.model.constraints Constraint])
  )

(s/defrecord CBIProblem [scope :- #{Variable}
                         semiring :- Semiring
                         constraints :- #{Constraint}])

(s/defn cbi-problem [semiring :- Semiring
                     constraints :- #{Constraint}]
  (let [scope (apply union (map :scope constraints))]
    (->CBIProblem scope semiring constraints)))

(s/defn apply-evidence-cbi [cbi evidence]
  (assert (subset? (into #{} (keys evidence))
                   (into #{} (map :name (:scope cbi))))
          "Provided evidence must be a subset of the scope of the cbi problem")
  (let [new-constraints (into #{} (map #(apply-evidence % evidence) (:constraints cbi)))
        new-scope (apply union (map :scope new-constraints))]
    (cbi-problem new-scope (:semiring cbi) new-constraints)))

;; Form a specific query against a cbi problem
(s/defn query
  ([cbi
    inference-alg
    query-assign]
   (let [inferred-cbi (inference-alg cbi (keys query-assign))]
     (calculate inferred-cbi query-assign)
     ))
  ([cbi
    inference-alg
    query-assign
    evidence]
   (let [inferred-cbi (inference-alg cbi (union (keys query-assign) (keys evidence)))]
     (if evidence
       (calculate (apply-evidence inferred-cbi evidence) query-assign)
       (calculate inferred-cbi query-assign))
     ))
  )
