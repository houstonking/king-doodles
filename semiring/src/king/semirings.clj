(ns king.semirings
  (:require [schema.core :as s]))


(s/defrecord Monoid [id op]
  clojure.lang.IFn
  (invoke [this] id)
  (invoke [this a b] (op a b)))

(s/defn monoid [id op]
  (->Monoid id op))

(s/defrecord Group [id ->inv op]
  clojure.lang.IFn
  (invoke [this] id)
  (invoke [this a] (->inv a))
  (invoke [this a b] (op a b)))

(s/defn group [id ->inv op]
  (->Group id ->inv op))

(s/defrecord Semiring [plus times])

(s/defn plus-id [sr]
  ((:plus sr)))

(s/defn times-id [sr]
  ((:times sr)))

(s/defrecord K-Semiring [monoids])

(defn semiring [plus times]
  (->Semiring plus times))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def bool-or (group false not (fn [a b] (or a b))))

(def bool-and (group true not (fn [a b] (and a b))))

(def bool-under-or-and (semiring bool-or bool-and))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def r-star-add (monoid 0 +))

(def r-star-mult (monoid 1 *))

(def r-star-under-add-mult (semiring r-star-add r-star-mult))
