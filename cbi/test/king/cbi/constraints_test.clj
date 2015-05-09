(ns king.cbi.constraints-test
  (:require [clojure.test :refer :all]
            [plumbing.core :refer [defnk fnk]]
            [schema.test]
            [king.cbi.model.constraints :refer :all]
            [king.cbi.model.variables :refer :all]
            [king.cbi.ops :refer :all]
            [king.cbi.model.cbi-problem :refer :all]
            [king.semirings :refer [times bool-under-or-and
                                    r-star-under-add-mult]]
            [king.cbi.algs.ve :refer :all]
            ))

(use-fixtures :once schema.test/validate-schemas)

(def bool (make-discrete-domain [true false]))

(def two-states (make-discrete-domain [0 1]))
(def three-states (make-discrete-domain [0 1 2]))

(def w (variable :w bool))
(def x (variable :x bool))
(def y (variable :y bool))
(def z (variable :z bool))

(def w-and-x (constraint #{w x}
                         (fnk [w x] (and w x))))
(def x-and-y (constraint #{x y}
                         (fnk [x y] (and x y))))
(def y-and-z (constraint #{y z}
                         (fnk [y z] (and y z))))

(def x-and-y-*and*-y-and-z (combine bool-under-or-and
                                    x-and-y
                                    y-and-z))

(def bool-marg (partial marginalize bool-under-or-and))

(def arity-0-constraint (-> x-and-y-*and*-y-and-z
                            (bool-marg x)
                            (bool-marg y)
                            (bool-marg z)
                            ))

(def bool-problem (cbi-problem #{w x y}
                               bool-under-or-and
                               #{w-and-x x-and-y}))

(def A (variable :A two-states))
(def B (variable :B three-states))

(def a-cpt (constraint #{A}
                       (fnk [A] ( {0 1
                                   1 0} A  ))))
(def b-cpt (constraint #{A B}
                       ;; P( B | A )
                       (fnk [A B] ({[0 0] 0.333
                                    [0 1] 0.333
                                    [0 2] 0.333
                                    [1 0] 0.333
                                    [1 1] 0.333
                                    [1 2] 0.333
                                    } [A B]) ) ))
(def a-->b (cbi-problem #{A B}
                        r-star-under-add-mult
                        #{a-cpt b-cpt}))
