(ns king.cbi.constraints-test
  (:require [clojure.test :refer :all]
            [plumbing.core :refer [defnk fnk for-map]]
            [schema.test]
            [king.cbi.model.constraints :refer :all]
            [king.cbi.model.variables :refer :all]
            [king.cbi.model.cbi-problem :refer :all]
            [king.cbi.model.semirings :refer [times
                                    or-and-sr
                                    add-mult-sr]]
            [king.cbi.algs.ve :refer :all]
            [taoensso.timbre :refer [spy debug]]
            [clojure.pprint :refer [pprint]]
            ))

(use-fixtures :once schema.test/validate-schemas)

(def bool [true false])

(def two-states [0 1])
(def three-states [0 1 2])

(def w (variable :w bool))
(def x (variable :x bool))
(def y (variable :y bool))
(def z (variable :z bool))

(def w-and-x-var (variable :w-and-x bool))
(def x-and-y-var (variable :x-and-y bool))
(def y-and-z-var (variable :y-and-z bool))

(def w-and-x (constraint #{w x}
                   #{w-and-x-var}
                   or-and-sr
                   (fnk [w x] [(and w x)])))

(def x-and-y (constraint #{x y}
                         #{x-and-y-var}
                         or-and-sr
                         (fnk [x y] [(and x y)])))

(def y-and-z (constraint #{y z}
                         #{y-and-z-var}
                         or-and-sr
                         (fnk [y z] [(and y z)])))




(def x-and-y-*and*-y-and-z (combine x-and-y
                                    y-and-z))



  (def bool-marg (partial marginalize or-and-sr))


  (def bool-problem (cbi-problem or-and-sr
                                 #{w-and-x x-and-y}))

(defn make-cpt [vars conds vals]
  (assert (every? sorted? [vars conds]))
  (let [var-states (all-assignments vars)
        cond-states (all-assignments conds)
        dists (partition (count var-states) vals)
        inner (for [d dists]
                (zipmap var-states d))
        _ (assert (= (count cond-states)
                     (/ (count vals)
                        (count var-states))))
        cpt (zipmap cond-states inner)]
    (constraint conds
                vars
                add-mult-sr
                cpt)
    ))




(def A (variable :A two-states ))
(def B (variable :B three-states ))
(def C (variable :C two-states))

(def a-cpt (make-cpt (sorted-var-set A) (sorted-var-set) [0.2 0.8]))

(def b-cpt (make-cpt (sorted-var-set B)
                     (sorted-var-set A)
                     [0.1
                      0.2
                      0.7

                      0.4
                      0.5
                      0.1]))

(def A-->B (cbi-problem #{A B}
                        #{a-cpt b-cpt}
                        ))
