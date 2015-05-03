(ns king.cbi
  (:require [schema.core :as s])
  )

(defrecord Variable [state domain])



(s/defrecord CBIProblem [variables
                         domains
                         semiring
                         constraints])

(defn combine-constraints [semiring c1 c2]
  (let [scope-c1 (scope c1)
        scope-c2 (scope c2)
        ])
  )
