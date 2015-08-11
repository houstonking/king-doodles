(ns king.cbi.ops
  (:require [schema.core :as s]
            [clojure.set :refer [intersection union difference]]
            [plumbing.core :as pc]
            [king.cbi.model.constraints :refer :all]
            [king.cbi.model.variables :refer :all]
            [king.semirings :refer :all]
            [taoensso.timbre :refer [spy debug]])
  (:import [king.cbi.model.constraints Constraint]
           [king.cbi.model.variables Variable]
           [king.semirings Semiring]))

(s/defn combine :- Constraint
  ([sr :- Semiring]
   (monoid
    (constraint #{} #(times-id sr))
    #(combine sr %1 %2 )))
  ([sr :- Semiring
    c1 :- Constraint
    c2 :- Constraint]
   (let [times (:times sr)]
     (constraint (union (:scope c1) (:scope c2))
                 #(times (c1 %) (c2 %))))))

(s/defn combine-all :- Constraint
  [sr :- Semiring
   cs :- [Constraint]]
  (reduce (combine sr) cs ))

(s/defn marginalize :- Constraint
  [sr :- Semiring
   cst :- Constraint
   var :- Variable]
  (assert (contains? (:scope cst) var))
  (let [plus (:plus sr)
        new-scope (difference (:scope cst) #{var})
        new-fn (fn [m]
                 (->> (for [s (states var)]  (merge m {(:name var) s}))
                      (map cst)
                      (reduce plus)))]
    (constraint new-scope new-fn) ))

(s/defn marginalize-all :- Constraint
  [sr :- Semiring
   constraint :- Constraint
   vars :- [Variable]]
  (reduce (partial marginalize sr) constraint vars ))

(s/defn memoize-constraint :- Constraint
  [cst :- Constraint]
  ::NYI)

;; Returns a map of all [assignment value] pairs for the given
;; constraint
(s/defn calculate-all-states
  [cst]
  (let [vars (:scope cst)
        assigns (all-assignments vars)]
    (pc/map-from-keys cst assigns)))
