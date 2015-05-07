(ns king.cbi.constraints
  (:require [schema.core :as s]

            [king.cbi.variables :refer [states]]
            [king.semirings :refer [times plus plus-identity]]

            [clojure.set :refer [intersection union difference]]

            [taoensso.timbre :refer [spy debug]])
  (:import [king.cbi.variables
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

(s/defn calculate [cst xs]
  ((:function cst) xs))

(s/defn apply-evidence :- Constraint
  [cst :- Constraint
   xs]
  (let [new-scope (->> (scope cst)
                       (filter (fn [v] (not (contains? (into #{} (keys xs)) (:name v)))  ))
                       (into #{}))
        new-fn (fn [m] (calculate cst (merge m xs)))]
    (map->Constraint {:scope new-scope :function new-fn})))

(s/defn combine :- Constraint
  [sr :- Semiring
   c1 :- Constraint
   c2 :- Constraint]
  (let [op (times sr)
        new-scope (union (scope c1) (scope c2))
        new-fn (fn [m] (op (calculate c1 m)
                           (calculate c2 m)))]
    (map->Constraint {:scope new-scope
                      :function new-fn})))

(s/defn combine-all :- Constraint
  [sr :- Semiring
   cs :- [Constraint]]
  (reduce (partial combine sr) cs ))

(s/defn marginalize :- Constraint
  [sr :- Semiring
   constraint :- Constraint
   var :- Variable]
  (assert (contains? (scope constraint) var))
  (let [op (plus sr)
        id (plus-identity sr)
        new-scope (difference (scope constraint) #{var})
        new-fn (fn [m]
                 (let [sub-states (for [s (states var)]
                                    (apply #(merge m %) {(:name var) s}))]
                   (reduce op id (map (partial calculate constraint) sub-states))))]
    (map->Constraint {:scope new-scope :function new-fn})))

(s/defn marginalize-all :- Constraint
  [sr :- Semiring
   constraint :- Constraint
   vars :- [Variable]]
  (reduce (partial marginalize sr) constraint vars ))


(s/defrecord CBIProblem [variables :- [Variable]
                         domains :-
                         semiring
                         constraints])
