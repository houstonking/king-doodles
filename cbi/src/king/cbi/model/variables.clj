(ns king.cbi.model.variables
  (:require [schema.core :as s]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [taoensso.timbre :refer [spy debug]]
            )
  )

(s/defrecord Variable [name domain])

(s/defn variable [name domain]
  (map->Variable {:name name :domain domain}))

(s/defn states [var]
  (:states (:domain var)))


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

(s/defrecord DiscreteDomain [states])

(s/defn make-discrete-domain [states]
  (map->DiscreteDomain {:states states})
  )
