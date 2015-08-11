(ns king.cbi.constraints-test
  (:require [clojure.test :refer :all]
            [plumbing.core :refer [defnk fnk]]
            [schema.test]
            [loom.io :as li]
            [king.cbi.model.constraints :refer :all]
            [king.cbi.model.variables :refer :all]
            [king.cbi.ops :refer :all]
            [king.cbi.model.cbi-problem :refer :all]
            [king.cbi.algs.jt :refer :all]
            [king.semirings :refer [bool-under-or-and
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

#_(def arity-0-constraint (-> x-and-y-*and*-y-and-z
                            (bool-marg x)
                            (bool-marg y)
                            (bool-marg z)
                            ))

(def bool-problem (cbi-problem #{w x y}
                               bool-under-or-and
                               #{w-and-x x-and-y}))

(def A (variable :A two-states))
(def B (variable :B three-states))
(def C (variable :C two-states))

(def a-cpt (constraint #{A}
                       (fnk [A] ( {[0] 2/5
                                   [1] 3/5} [A]  ))))
(def b-cpt (constraint #{A B}
                       ;; P( B | A )
                       (fnk [A B] ({[0 0] 1/2
                                    [0 1] 1/3
                                    [0 2] 1/6
                                    [1 0] 1/6
                                    [1 1] 1/2
                                    [1 2] 1/3} [A B]))))

(def c-cpt (constraint #{B C}
                       ;; P ( C | B )
                       (fnk [B C] ({[0 0] 1/8
                                    [0 1] 1/4
                                    [1 0] 5/8
                                    [1 1] 5/8
                                    [2 0] 1/8
                                    [2 1] 1/4} [B C]) )
                       )
  )

(def a-->b (cbi-problem #{A B}
                        r-star-under-add-mult
                        #{a-cpt b-cpt}))

(def a-->b-->c (cbi-problem #{A B C}
                            r-star-under-add-mult
                            #{a-cpt b-cpt c-cpt}))


(def v1 (variable :v1 two-states))
(def v2 (variable :v2 two-states))
(def v3 (variable :v3 two-states))
(def v4 (variable :v4 two-states))
(def v5 (variable :v5 two-states))

(def f1 (constraint #{v1 v2 v3}
                    (fnk [v1 v2 v3] (+ v1 v2 v3) )
                    ))

(def f2 (constraint #{v2 v3 v4}
                    (fnk [v2 v3 v4] (+ v2 v3 v4))))

(def f3 (constraint #{v3 v5}
                    (fnk [v3 v5] (+ v3 v5))))

(def lech (cbi-problem #{v1 v2 v3 v4 v5}
                       r-star-under-add-mult
                       #{f1 f2 f3}))
