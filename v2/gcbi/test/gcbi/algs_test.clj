(ns gcbi.core-test
  (:require [clojure.test :refer :all]
            [gcbi.algs :refer :all]
            [loom.graph :as lg]
            [loom.io :as lio]))

(deftest csp-test
  (let [sr (->semiring {:+ (fn [x y] (or x y))
                        :* (fn [x y] (and x y))})
        a (->variable '?a 1 2 3 4)
        b (->variable '?b 1 2 3 4)
        c (->variable '?c 1 2 3 4)
        d (->variable '?d 1 2 3 4)
        e (->variable '?e 1 2 3 4)
        c1  (->constraint sr #{b}   (fn [m] (not= ('?b m) 3)))
        c2  (->constraint sr #{c}   (fn [m] (not= ('?c m) 3)))
        c3  (->constraint sr #{a b} (fn [m] (not= ('?a m) ('?b m))))
        c4  (->constraint sr #{b c} (fn [m] (not= ('?b m) ('?c m))))
        c5  (->constraint sr #{c d} (fn [m] (< ('?c m) ('?d m))))
        c6  (->constraint sr #{a d} (fn [m] (= ('?a m) ('?d m))))
        c7  (->constraint sr #{e a} (fn [m] (< ('?e m) ('?a m))))
        c8  (->constraint sr #{e b} (fn [m] (< ('?e m) ('?b m))))
        c9  (->constraint sr #{e c} (fn [m] (< ('?e m) ('?c m))))
        c10 (->constraint sr #{e d} (fn [m] (< ('?e m) ('?d m))))
        c11 (->constraint sr #{b d} (fn [m] (not= ('?b m) ('?d m))))

        cbip (->cbi-problem #{c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11})

        cx (combine c1 c2)
        cm (marginalize cx b)]
    (testing "constraints"
      (are [f x y] (= (f x) y)
        c1 {'?b 1} true
        c1 {'?b 3} false

        cx {'?b 0 '?c 0} true
        cx {'?b 0 '?c 3} false
        cx {'?b 3 '?c 0} false
        cx {'?b 3 '?c 3} false
        cm {'?c 0} true
        cm {'?c 3} false))))

(deftest weighted-csp-test
  (let [sr (->semiring  {:+ (fn [x y] (max x y))
                         :* (fn [x y] (+ x y))})
        a (->variable '?a 1 2 3 4)
        b (->variable '?b 1 2 3 4)
        c (->variable '?c 1 2 3 4)
        d (->variable '?d 1 2 3 4)
        e (->variable '?e 1 2 3 4)
        c1  (->constraint sr #{b}   (fn [m] (if (not= ('?b m) 3) 1 0)))
        c2  (->constraint sr #{c}   (fn [m] (if (not= ('?c m) 3) 1 0)))
        c3  (->constraint sr #{a b} (fn [m] (if (not= ('?a m) ('?b m)) 1 0)))
        c4  (->constraint sr #{b c} (fn [m] (if (not= ('?b m) ('?c m)) 1 0)))
        c5  (->constraint sr #{c d} (fn [m] (if (< ('?c m) ('?d m)) 1 0)))
        c6  (->constraint sr #{a d} (fn [m] (if (= ('?a m) ('?d m)) 1 0)))
        c7  (->constraint sr #{e a} (fn [m] (if (< ('?e m) ('?a m)) 1 0)))
        c8  (->constraint sr #{e b} (fn [m] (if (< ('?e m) ('?b m)) 1 0)))
        c9  (->constraint sr #{e c} (fn [m] (if (< ('?e m) ('?c m)) 1 0)))
        c10 (->constraint sr #{e d} (fn [m] (if (< ('?e m) ('?d m)) 1 0)))
        c11 (->constraint sr #{b d} (fn [m] (if (not= ('?b m) ('?d m)) 1 0)))

        cbip (->cbi-problem #{c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11})

        cx (combine c1 c2)
        cm (marginalize cx b)]

    (testing "constraints"
      (are [f x y] (= (f x) y)
        c1 {'?b 1} 1
        c1 {'?b 3} 0

        cx {'?b 0 '?c 0} 2
        cx {'?b 0 '?c 3} 1
        cx {'?b 3 '?c 0} 1
        cx {'?b 3 '?c 3} 0
        cm {'?c 0} 2
        cm {'?c 3} 1))

    (is (= {1 11, 2 8, 3 76, 4 141, 5 191, 6 210, 7 203, 8 113, 9 54, 10 16, 11 1}
           (frequencies (map (brute-inference cbip [a b c d e]) (state-space [a b c d e])))))

    (println (constraints cbip))
    (println (approx-marginalize (constraints cbip) b 3 3))
    (println (map (brute-inference cbip [e]) (state-space [e])))

    (println (junction-tree cbip [a b c d e]))))

(deftest junction-tree-test
  (let [sr (->semiring {:+ (fn [x y] (or x y))
                        :* (fn [x y] (and x y))})
        a (->variable '?a 1 2 3 4)
        b (->variable '?b 1 2 3 4)
        c (->variable '?c 1 2 3 4)
        d (->variable '?d 1 2 3 4)
        e (->variable '?e 1 2 3 4)
        f (->variable '?f 1 2 3 4)
        g (->variable '?g 1 2 3 4)
        f1 (->constraint sr #{a b}
                         (fn [m]
                           (cond-> 0
                             (= ('?a m) ('?b m)) inc)))
        f2 (->constraint sr #{b c}
                         (fn [m]
                           (cond-> 0
                             (= ('?b m) ('?c m)) inc)))
        f3 (->constraint sr #{b d}
                         (fn [m]
                           (cond-> 0
                             (= ('?b m) ('?c m)) inc)))
        f4 (->constraint sr #{c e}
                         (fn [m]
                           (cond-> 0
                             (not= ('?c m) ('?e m)) inc
                             )))

        f5 (->constraint sr #{e f}
                         (fn [m]
                           (cond-> 0
                             (not= ('?e m) ('?f m)) inc)))

        f6 (->constraint sr #{d f}
                         (fn [m]
                           (cond-> 0
                             (not= ('?d m) ('?f m)) inc)))
        f7 (->constraint sr #{d g}
                         (fn [m]
                           (cond-> 0
                             (not= ('?d m) ('?g m)) inc)))
        f8 (->constraint sr #{f g}
                         (fn [m]
                           (cond-> 0
                             (not= ('?f m) ('?g m)) inc)))
        f9 (->constraint sr #{c f}
                         (fn [m]
                           (cond-> 0
                             (not= ('?c m) ('?f m)) inc)))

        cbip (->cbi-problem #{f1 f2 f3 f4 f5})]
    (lio/view (let [order [a b c d e f g]]
                (junction-tree cbip order)
                ))
    )
  )

(deftest junction-tree-test-2
  (let [sr (->semiring {:+ (fn [x y] (+ x y))
                        :* (fn [x y] (* x y))})
        v1 (->variable '?v1 1 2 3 4)
        v2 (->variable '?v2 1 2 3 4)
        v3 (->variable '?v3 1 2 3 4)
        v4 (->variable '?v4 1 2 3 4)
        v5 (->variable '?v5 1 2 3 4)
        f1 (->constraint sr #{v1 v2 v3} (fn [m]
                                          (cond-> 0
                                            (not= ('?v1 m) ('?v2 m)) inc
                                            (not= ('?v1 m) ('?v3 m)) inc
                                            (not= ('?v2 m) ('?v3 m)) inc)))
        f2 (->constraint sr #{v2 v3 v4} (fn [m]
                                          (cond-> 0
                                            (not= ('?v2 m) ('?v3 m)) inc
                                            (not= ('?v2 m) ('?v4 m)) inc
                                            (not= ('?v3 m) ('?v5 m)) inc)))
        f3 (->constraint sr #{v3 v5} (fn [m]
                                       (cond-> 0
                                         (= ('?v3 m) ('?v5 m)) (+ 3))))
        cbip (->cbi-problem #{f1 f2 f3})]
    (lio/view (junction-tree cbip (shuffle [v3 v1 v5 v2 v4])))
    ))

(deftest junction-tree-3
  (let [sr (->semiring {:+ (fn [x y] (+ x y))
                        :* (fn [x y] (* x y))})
        a (->variable 'a 1 2 3)
        b (->variable 'b 1 2 3)
        c (->variable 'c 1 2 3)
        d (->variable 'd 1 2 3 4)
        e (->variable 'e 1 2 3 4)
        f (->variable 'f 1 2 3 4)

        fab (->constraint sr #{a b} (fn [m]))
        fac (->constraint sr #{a c} (fn [m]))
        fbd (->constraint sr #{b d} (fn [m]))
        fbe (->constraint sr #{b e} (fn [m]))
        fbf (->constraint sr #{b f} (fn [m]))
        fce (->constraint sr #{c e} (fn [m]))
        fef (->constraint sr #{e f} (fn [m]))

        cbip (->cbi-problem #{fab fac fbd fbe fbf fce fef})]
    (is (= #{#{a b c} #{b d} #{e b f} #{e b c}}
           (lg/nodes (junction-tree cbip [f d e c b a])))))
  )

(deftest misconception-network
  (let [sr (->semiring {:+ (fn [x y] (+ x y))
                        :* (fn [x y] (* x y))})
        a (->variable 'a 0 1)
        b (->variable 'b 0 1)
        c (->variable 'c 0 1)
        d (->variable 'd 0 1)

        fab (->constraint sr
                          #{a b}
                          (fn [m]
                            (case [('a m) ('b m)]
                              [0 0] 30
                              [0 1] 5
                              [1 0] 1
                              [1 1] 10)))

        fbc (->constraint sr
                          #{b c}
                          (fn [m]
                            (case [('b m) ('c m)]
                              [0 0] 100
                              [0 1] 1
                              [1 0] 1
                              [1 1] 100)))

        fcd (->constraint sr
                          #{c d}
                          (fn [m]
                            (case [('c m) ('d m)]
                              [0 0] 1
                              [0 1] 100
                              [1 0] 100
                              [1 1] 1)))

        fda (->constraint sr #{d a}
                          (fn [m]
                            (case [('d m) ('a m)]
                              [0 0] 100
                              [0 1] 1
                              [1 0] 1
                              [1 1] 100)))

        cbip (->cbi-problem #{fab fbc fcd fda})
        Z 7201840]
    (println ((brute-inference cbip []) {}))
    (clojure.pprint/pprint (zipmap (state-space [a b c d]) (map (brute-inference cbip [a b c d]) (state-space [a b c d]))))
    (println (double (/ ((brute-inference cbip [b]) {'b 0}) Z)))
    (println (double (/ ((brute-inference cbip [b]) {'b 1}) Z)))
    ))
