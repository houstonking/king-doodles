(ns king.cbi.model.constraints
  (:require [schema.core :as s]
            [clojure.set :refer [union
                                 difference]]
            [king.cbi.model.variables :refer [all-assignments
                                              union-var-sets
                                              states]]
            [king.cbi.model.semirings :refer [times
                                              plus
                                              plus-identity]]
            [taoensso.timbre :refer [spy debug]]
            )
  (:import [king.cbi.model.semirings Semiring]
           [king.cbi.model.variables Variable]
           )
  )

(s/defrecord Constraint [in-scope
                         out-scope
                         semiring
                         function])

(s/defn constraint [in-scope out-scope semiring function]
  (map->Constraint {:in-scope in-scope
                    :out-scope out-scope
                    :semiring semiring
                    :function function}))

;; return the submap st all keys are names in the var set
(s/defn intersect-keys-names
  [var-set var-map]
  (let [names (into #{} (map :name var-set))]
    (into {} (filter (fn [[k v]] (contains? names k)) var-map))))

(defn calculate [cst xs]
  (debug "Calculating" cst xs)
  (let [in-vars  (intersect-keys-names (:in-scope cst) xs )
        out-vars (intersect-keys-names (:out-scope cst) xs)]
    (when (seq (:in-scope cst))
      (assert (= (spy (keys in-vars))
                 (spy (map :name (:in-scope cst)))) "Missing keys from the in-scope of the constraint"))
    (when (seq (:out-scope cst))
      (assert (= (keys out-vars)
                 (map :name (:out-scope cst))) "Missing keys from the out-scope of the constraint"))
    (spy [in-vars out-vars])
    (((:function cst) in-vars) out-vars))
  )


(s/defn combine :- Constraint
  [c1 :- Constraint
   c2 :- Constraint]
  (let [sr (:semiring c1)
        op (times sr)
        new-out-scope (union-var-vecs (:out-scope c1)
                                      (:out-scope c2))
        new-in-scope (difference (union-var-vecs (:in-scope c1)
                                                 (:in-scope c2))
                                 new-out-scope)

        ;; This needs to be made to respect inner-outer rules
        new-fn (fn [m]
                 (op (spy (calculate (spy c1) m))
                     (spy (calculate c2 m)))
                 )

        ]
    (constraint new-in-scope
                new-out-scope
                sr
                new-fn
                )))

(s/defn combine-all :- Constraint
  [cs :- [Constraint]]
  (reduce combine cs ))

(s/defn marginalize :- Constraint
  [constraint :- Constraint
   var :- Variable]
  (assert (contains? (:scope constraint) var))
  (let [sr (:semiring constraint)
        op (plus sr)
        id (plus-identity sr)
        new-scope (difference (:scope constraint) #{var})
        new-fn (fn [m]
                 (let [sub-states (for [s (states var)]
                                    (apply #(merge m %) {(:name var) s}))]
                   (reduce op id (map (partial calculate constraint) sub-states))))]
    (constraint new-scope sr new-fn)

    ))

(s/defn marginalize-all :- Constraint
  [constraint :- Constraint
   vars :- [Variable]]
  (reduce marginalize constraint vars ))

(s/defn memoize-constraint :- Constraint
  [cst :- Constraint]
  ::NYI)

;; Returns a lazy seq of all [assignment value] pairs for the given
;; constraint
(s/defn calculate-all-states
  [cst]
  (let [vars (:in-scope cst)
        assigns (all-assignments vars)]
    (for [assign assigns]
      [assign (calculate cst assign)])))
