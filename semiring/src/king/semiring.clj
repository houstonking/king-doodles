(ns king.semirings
  (:require [schema.core :as s]))

(s/defrecord Semigroup [member?
                        op
                        commutative?])
(s/defrecord Monoid [member?
                     op
                     id
                     commutative?])
(s/defrecord Group [member?
                    op
                    id
                    inverse
                    commutative?])

(s/defrecord Semiring [plus-monoid
                       times-monoid
                       commutative?])

(defn semiring [plus times]
  (let [commutative? (and (:id plus)
                          (:id times)
                          (:commutative? plus)
                          (:commutative? times))]
    (map->Semiring {:plus-monoid plus
                    :times-monoid times
                    :commutative? commutative?})
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn boolean? [x]
  (= (type x) Boolean))

(def BooleanOr (->Group boolean?
                        (fn [x y] (or x y))
                        false
                        not
                        true
                        ))

(def BooleanAnd (->Group boolean?
                         (fn [x y] (and x y))
                         true
                         not
                         true
                         ))

(def BooleanOrAnd (semiring BooleanOr BooleanAnd))
