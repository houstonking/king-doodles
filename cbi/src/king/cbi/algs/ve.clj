(ns king.cbi.algs.ve
  (:require [schema.core :as s]
            [clojure.set :refer [difference]]
            [king.cbi.ops :refer :all]

            )

  )
;; assignment task is a filter over the returned constraint
(s/defn brute-inference
  [cbi({[0 0] 0.5
                                    [0 1] 0.2
                                    [0 2] 0.3
                                    [1 0] 0.1
                                    [1 1] 0.1
                                    [1 2] 0.8
                                    }) [A B] ) ))
   query-vars]
  (let [sr (:semiring cbi)
        nuisances (filter (fn [v] (not (contains? query-vars v)))
                          (:scope cbi))]
    (marginalize-all sr
                     (combine-all sr (:constraints cbi))
                     nuisances)))

(s/defn variable-elimination
  [cbi
   query-vars
   elim-ordering-fn]
  (let [sr (:semiring cbi)
        nuisances (filter (fn [v] (not (contains? query-vars v)))
                          (:scope cbi))
        ordered-nuis (elim-ordering-fn cbi nuisances)
        ]
 ))
