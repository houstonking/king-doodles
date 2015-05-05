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


(s/defn times [sr]
  (-> sr
      :times-monoid
      :op))

(s/defn plus [sr]
  (-> sr
      :plus-monoid
      :op))

(s/defn plus-identity [sr]
  (-> sr
      :plus-monoid
      :id)
  )

(s/defrecord K-Semiring [monoids])

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

(defn bool? [x]
  (= (type x) Boolean))

(def bool-or (->Group bool?
                        (fn [x y] (or x y))
                        false
                        not
                        true
                        ))

(def bool-and (->Group bool?
                         (fn [x y] (and x y))
                         true
                         not
                         true
                         ))

(def bool-under-or-and (semiring bool-or bool-and))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rstar? [x]
  (>= x 0))

(def RStarAdd (->Monoid rstar?
                            +
                            0
                            true))
(def RStarMult (->Monoid rstar?
                         *
                         1
                         true))
(def RStarUnderAddMult (semiring RStarAdd RStarMult))
