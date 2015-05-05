(ns king.cbi.constraints-test
  (:require [clojure.test :refer :all]
            [plumbing.core :refer [defnk fnk]]
            [schema.test]
            [king.cbi.constraints :refer :all]
            [king.cbi.variables :refer [make-discrete-domain
                                        variable
                                        ]]
            [king.semirings :refer [times bool-under-or-and]]
            ))

(use-fixtures :once schema.test/validate-schemas)

(def bool (make-discrete-domain [true false]))

(def x (variable :x bool))

(def y (variable :y bool))

(def z (variable :z bool))

(def x-and-y (constraint #{x y} (fnk [x y] (and x y))))

(def y-and-z (constraint #{y z} (fnk [y z] (and y z))))

(def x-and-y-*and*-y-and-z (combine bool-under-or-and
                                    x-and-y
                                    y-and-z))

(def bool-marg (partial marginalize bool-under-or-and))

(def arity-0-constraint (-> x-and-y-*and*-y-and-z
                            (bool-marg x)
                            (bool-marg y)
                            (bool-marg z)
                            ))
