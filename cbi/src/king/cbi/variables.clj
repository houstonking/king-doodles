(ns king.cbi.variables
  (:require [schema.core :as s]
            )
  )

(s/defrecord Variable [name domain])

(s/defn variable [name domain]
  (map->Variable {:name name :domain domain}))

(s/defn states [var]
  (:states (:domain var)))


(defmethod print-method Variable
  [o w]
  (print-simple (str (:name o)) w)
  )


(s/defn state-assignment [vars :- [Variable]
                          states :- [s/Any]]
  (assert (= (count vars) (count states)))
  (zipmap vars states)
  )

(s/defrecord DiscreteDomain [states])

(s/defn make-discrete-domain [states]
  (map->DiscreteDomain {:states states})
  )
