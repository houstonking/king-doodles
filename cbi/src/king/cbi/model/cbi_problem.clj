(ns king.cbi.model.cbi-problem
  (:require [schema.core :as s]
            [king.semirings :as sr]
            [king.cbi.model.variables ]
            [king.cbi.model.constraints]
            [king.cbi.ops :refer [apply-evidence]]
            [clojure.set :refer [union subset?]]
            [taoensso.timbre :refer [spy]])

  (:import [king.semirings Semiring]
           [king.cbi.model.variables Variable]
           [king.cbi.model.constraints Constraint])
  )

(s/defrecord CBIProblem [scope :- #{Variable}
                         semiring :- Semiring
                         constraints :- #{Constraint}])

(s/defn ^:always-validate cbi-problem [scope :- #{Variable}
                                       semiring :- Semiring
                                       constraints :- #{Constraint}]
  (assert (= scope (apply union (map :scope constraints))))
  (->CBIProblem scope semiring constraints))

(s/defn apply-evidence-cbi [cbi evidence]
  (assert (subset? (into #{} (keys evidence))
                   (into #{} (map #(:name %) (:variables cbi))))
          "Provided evidence must be a subset of the scope of the cbi problem")
  (let [new-constraints (into #{} (map #(apply-evidence % evidence) (:constraints cbi)))
        new-scope (apply union (map :scope new-constraints))]
    (cbi-problem new-scope (:semiring cbi) new-constraints)
    )
  )
