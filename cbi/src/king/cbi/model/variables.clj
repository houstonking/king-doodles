(ns king.cbi.model.variables
  (:require [schema.core :as s]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [taoensso.timbre :refer [spy debug]]
            )
  )

(s/defrecord Variable [name :- s/Keyword
                       domain :- [s/Any]])

(s/defn variable [name domain]
  (map->Variable {:name name :domain domain}))

(s/defn states [var]
  (:domain var))

(defmethod print-method Variable
  [o w]
  (print-simple (str "var" (:name o)) w))

(s/defn state-assignment [vars :- [Variable]
                          states :- [s/Any]]
  (assert (= (count vars) (count states)))
  (zipmap vars states)
  )

(s/defn all-assignments [vars]
  (let [domains (for [v vars]
                  (for [state (states v)]
                     {(:name v) state}))
        assigns (map #(into {} %)  (apply cartesian-product domains))]
    assigns))

(s/defn var-name-comparitor
  [v1 v2]
  (compare [(:name v1)]
           [(:name v2)])
  )

(s/defn sorted-var-set
  [& vars]
  (apply sorted-set-by var-name-comparitor vars))

;; Returns the set of vars sorted by :name
(s/defn union-var-sets
  [vs1
   vs2]
  (apply sorted-var-set (concat vs1 vs2)))
