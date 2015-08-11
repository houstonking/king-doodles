(ns king.cbi.model.constraints
  (:require [schema.core :as s]
            [king.cbi.model.variables]
            [king.semirings]
            [taoensso.timbre :refer [spy debug]])
  (:import [king.cbi.model.variables
            Variable]
           [king.semirings
            Semiring]))

(s/defrecord Constraint
    [scope :- #{Variable}
     function]
  clojure.lang.IFn
  (invoke [this] (function) )
  (invoke [this input] (function input)))

(s/defn constraint [scope function]
  (->Constraint scope function))
