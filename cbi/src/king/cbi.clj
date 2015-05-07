(ns king.cbi
  (:require [schema.core :as s])
  )

(defrecord Variable [state domain])
