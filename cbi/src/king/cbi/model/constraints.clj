(ns king.cbi.model.constraints
  (:require [schema.core :as s]

            [king.cbi.model.variables :refer [states]]
            [king.semirings :refer [times plus plus-identity]]



            [taoensso.timbre :refer [spy debug]])
  (:import [king.cbi.model.variables
            Variable]
           [king.semirings
            Semiring]
           ))



(s/defrecord Constraint
    [scope :- #{Variable}
     function])

(s/defn constraint [scope function]
  (map->Constraint {:scope scope :function function}))

(s/defn scope [cst]
  (:scope cst))

(s/defrecord CBIProblem [variables :- [Variable]
                         semiring
                         constraints])
