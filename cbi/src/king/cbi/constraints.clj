(ns king.cbi.constraints
  (:require [schema.core :as s]
            [king.cbi.variables])
  (:import [king.cbi.variables
            Variable])
  )

(s/defrecord Constraint
    []
    )


(s/defn combine :- Constraint
  [c1 :- Constraint
   c2 :- Constraint])

(s/defn marginalize :- Constraint
  [c :- Constraint
   var :- Variable]
  )
