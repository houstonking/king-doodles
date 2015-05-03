(ns king.semiring
  (:require [schema.core :as s])
  )

(defprotocol IBooleanOperation
  (op [x y] "Defines the behavior of 'x op y'"))

(defprotocol IIsCommutative
  (commutative? [this] "True if the operation commutes"))

(defprotocol IIsAssociative
  (is-associative? [this] "True if the operation is associative"))

(defprotocol IIdentity
  (identity-element [this] "Returns the identity element for the object"))

(defprotocol IHasInverses
  (has-inverses? [this] "Returns true if the object is invertible"))

(defprotocol IIsMember
  (is-member? [this x] "Returns true if x is in this"))


(defn monoid? [obj]
  (assert (satisfies? obj IIsMember))
  (assert (satisfies? obj IIsAssociative))
  (assert (satisfies? obj IIdentity))
  (assert (and (closed? obj)
               (is-associative? obj)
               (identity-element obj))))

(defn commutative-monoid? [obj]
  (assert (satisfies? obj IIsCommutative))
  (assert (monoid? obj))
  (assert (commutative? obj)))

(s/defschema Monoid (s/pred monoid?))

(s/defschema CommutativeMonoid (s/pred commutative-monoid?))

(deftype BooleanOrMonoid []
  IBooleanOperation
  (op [x y] (or x y))

  IIsMember
  (is-member? [_ x] (= Boolean (type x)))

  IIsAssociative
  (is-associative? [_] true)

  IIs
  )


(s/defrecord Semiring
    [add :- CommutativeMonoid
     times :- Monoid])

(s/defn semiring [opts]
  (map->Semiring opts))

(defn has-detector?
  "Element x in R is a 'detector' if x is both:
   1) The absorbing element of *
   2) The identity element of +
   Detecting elements can be used to perform domain
   shrinking (e.g. Arc Consistency in CSP)"
  [sr] )




(def BooleanSR
  )
