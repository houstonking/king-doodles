(ns king.cbi.algs.ve
  (:require [schema.core :as s]
            [clojure.set :refer [difference]]
            [taoensso.timbre :refer [spy debug]]
            [king.cbi.model.constraints :refer [marginalize-all
                                                combine-all]]
            ))

;; assignment task is a filter over the returned constraint
(s/defn brute-inference
  [cbi
   query-var-names]
  (let [sr (:semiring cbi)
        nuisances (->> (:scope cbi)
                       ;; { x \in scope | x \not\in evidence }
                       (filter (fn [v] (not (contains? (into #{} query-var-names) (:name v)))))
                       (into #{}))
        _ (spy nuisances)]
    (marginalize-all
                     (combine-all (:constraints cbi))
                     nuisances)))

(s/defn variable-elimination
  [cbi
   query-vars
   elim-ordering-fn]
  (let [sr (:semiring cbi)
        nuisances (filter (fn [v] (not (contains? query-vars v)))
                          (:scope cbi))
        ordered-nuis (elim-ordering-fn cbi nuisances)
        ]))
