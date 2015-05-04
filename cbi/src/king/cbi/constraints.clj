(ns king.cbi.constraints
  (:require [schema.core :as s]
            [king.cbi.variables]
            [clojure.set :refer [intersection union]])
  (:import [king.cbi.variables
            Variable]))

(s/defrecord Constraint
    [scope :- #{Variable}
     function])

(s/defn combine :- Constraint
  [sr :- Semiring
   c1 :- Constraint
   c2 :- Constraint]
  (let [f1 (:function c1)
        f2 (:function c2)
        s1 (:scope c1)
        s2 (:scope c2)
        * (:times sr)
        new-scope (union s1 s2)]
    (letfn [(f-new [vars]
              (* (f1 (intersection vars s1))
                 (f2 (intersection vars s2))))]
      (map->Constraint {:scope new-scope :function f-new})
      )))




(s/defn marginalize :- Constraint
  [c :- Constraint
   var :- Variable]



  )
