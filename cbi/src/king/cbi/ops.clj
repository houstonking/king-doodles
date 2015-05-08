(ns king.cbi.ops
  (:require [schema.core :as s]
            [clojure.set :refer [intersection union difference]]

            [king.cbi.model.constraints :refer :all]
            [king.cbi.model.variables :refer :all]
            [king.semirings :refer :all]

            [taoensso.timbre :refer [spy debug]]
            )

  (:import [king.cbi.model.constraints Constraint]
           [king.cbi.model.variables Variable]
           [king.semirings Semiring])
  )

(s/defn calculate [cst xs]
  ((:function cst) xs))

(s/defn apply-evidence :- Constraint
  [cst :- Constraint
   evidence]
  (let [new-scope (->> (:scope cst)
                       ;; { x \in scope | x \not\in evidence }
                       (filter (fn [v] (not (contains? (into #{} (keys evidence)) (:name v)))))
                       (into #{}))
        new-fn (fn [m] (calculate cst (merge m evidence)))]
    (map->Constraint {:scope new-scope :function new-fn})))

(s/defn combine :- Constraint
  [sr :- Semiring
   c1 :- Constraint
   c2 :- Constraint]
  (let [op (times sr)
        new-scope (union (:scope c1) (:scope c2))
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
  (assert (contains? (:scope constraint) var))
  (let [op (plus sr)
        id (plus-identity sr)
        new-scope (difference (:scope constraint) #{var})
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

;; Returns a lazy seq of all [assignment value] pairs for the given
;; constraint
(s/defn calculate-all-states
  [cst]
  (let [vars (:scope cst)
        assigns (all-assignments vars)]
    (for [assign assigns]
      [assign (calculate cst assign)])))
