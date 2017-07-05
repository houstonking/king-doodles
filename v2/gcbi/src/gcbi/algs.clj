(ns gcbi.algs
  (:require [clojure.set :as cs]
            [clojure.math.combinatorics :as combo]
            [loom.graph :as lg]
            [loom.label :as ll]
            [loom.alg :as la]
            [loom.io :as lio]))

(defrecord Semiring [+ * max one zero])

(defn ->semiring
  ([m] (map->Semiring m)))

(defprotocol Variable
  (domain [variable] "Return the set of valid state assignments for the variable"))

(defrecord CBIVariable [name domain]
  Object
  (toString [_] (.toString name))
  Variable
  (domain [_] domain))

(defmethod print-method CBIVariable [v ^java.io.Writer w]
  (print-method (:name v) w))

(defn ->variable [name & states]
  (->CBIVariable name (into #{} (for [s states] {name s}))))

(defprotocol Constraint
  (sr [constraint] "Returns the semiring over which the constraint is defined")
  (scope [constraint] "Returns the scope of the constraint as a set of variables")
  (combine [c1 c2] "Returns a new constraint that is the combination of the two constraints.")
  (marginalize [constraint var] "Marginalizes the variable from the constraint.")
  (tabulate [constraint] "Flattens the constraint into a hasmap of all inputs to outputs"))

(defn sum-prod [constraints var]
  (marginalize (reduce combine constraints) var))

(deftype CBIConstraint [semiring
                        _scope
                        f]
  Object
  (toString [_] (format "f(%s)" (clojure.string/join ", " (for [s _scope] (.toString s)))))

  clojure.lang.IFn
  (invoke [_ args] (f args))

  Constraint
  (sr [_] semiring)
  (scope [_] _scope)
  (combine [c1 c2]
    (let [scope' (cs/union (scope c1) (scope c2))
          f' (fn [w] ((:* semiring) (c1 w) (c2 w)))]
      (CBIConstraint. semiring scope' f')))

  (marginalize [c var]
    (assert (contains? _scope var))
    (let [scope' (disj _scope var)
          f' (fn [w]
               (reduce (:+ semiring)
                       (for [v_i (domain var)]
                         (c (cs/union w v_i)))))]
      (CBIConstraint. semiring scope' f')))
  (tabulate [c]
    (let [ss (state-space _scope)
          table (into {} (for [s ss] [s (f s)]))]
      (CBIConstraint.
       semiring
       _scope
       table))))

(defn ->constraint [semiring scope f]
  (CBIConstraint. semiring scope f))

(defmethod print-method CBIConstraint [c ^java.io.Writer w]
  (.write w "f(")
  (doseq [s (scope c)] (print-method s w))
  (.write w ")"))

(defprotocol Problem
  (semiring [cbip])
  (variables [cbip])
  (constraints [cbip])
  (eliminate-variable [cbip var])
  (approx-eliminate-variable [cbip var b max-width])
  (brute-inference [cbip vars])
  (primal-graph [cbip])
  (factor-graph [cbip])
  (triangulated-primal-graph [cbip ordering])
  (junction-tree [cbip ordering]))

(defn approx-marginalize [constraints var b max-width]
  (let [grouped-constraints (group-by #(contains? (scope %) var) constraints)
        scoped-constraints (get grouped-constraints true)
        unscoped-constraints (get grouped-constraints false)
        subscope (reduce cs/union (map scope scoped-constraints))]
    (if (<= (dec (count subscope)) max-width)
      (into #{} (conj unscoped-constraints (sum-prod scoped-constraints var)))
      ;; TODO: generalize parition function / heuristics
      (let [partitions (partition b scoped-constraints)]
        (into #{}
              (concat
               unscoped-constraints
               (mapcat #(approx-marginalize % var b max-width) partitions)))))))

(defn triangulate
  ([g ordering]
   (let [neighbors (into {}
                         (for [node ordering]
                           [node (into #{} (map second (lg/out-edges g node)))]))]
     (triangulate g ordering #{} neighbors)))
  ([g ordering new-edges neighbors]
   (let [cur-var (first ordering)
         cur-neighbors (neighbors cur-var)
         cur-edges (into #{} (cs/union new-edges (lg/edges g)))
         non-chordal-neighbors (for [x cur-neighbors
                                     y cur-neighbors
                                     :when (and (not= x y)
                                                (not (contains? cur-edges [x y])))]
                                 [x y])
         new-edges (reduce conj new-edges non-chordal-neighbors)
         neighbors (reduce (fn [a [x y]]
                             (-> a
                                 (update-in [x] conj y)
                                 (update-in [y] conj x)))
                           neighbors
                           non-chordal-neighbors)
         neighbors (into {}
                    (for [[k v] neighbors]
                      [k (disj v cur-var)]))]
     (if (not-empty (rest ordering))
       (recur g (rest ordering) new-edges neighbors)
       (apply lg/graph (cs/union (lg/edges g) new-edges))))))


(defn tree-decomposition [graph ordering]
  ;; TODO: handle input graph with multiple connected components?
  (let [cliques (la/maximal-cliques graph)
        ;; two cliques have an edge if they share any nodes.
        ;; we weight them by - | overlapping-nodes | so
        ;; we can get the maximum spanning tree using prims
        clique-edges (for [c1 cliques
                           c2 cliques
                           :when (and (not= c1 c2)
                                      (not-empty (cs/intersection c1 c2)))]
                       [c1 c2 (- (count (cs/intersection c1 c2)))])]
    (if-not (empty? clique-edges) ;; single clique
      (let [clique-graph (if-not (empty? clique-edges)
                           (reduce lg/add-edges (lg/weighted-graph) clique-edges)
                           (lg/add-nodes (lg/weighted-graph) (set ordering)))
            mst (la/prim-mst clique-graph)]
        (reduce (fn [g [n1 n2 _]]
                  (ll/add-labeled-edges g [n1 n2] (cs/intersection n1 n2)))
                (lg/graph)
                (lg/edges mst)))
      (lg/add-nodes (lg/graph) (set ordering)))))

(deftype CBIProblem [semiring
                     variables
                     constraints]
  Problem
  (semiring [cbip] semiring)
  (variables [cbip] variables)
  (constraints [cbip] constraints)

  (primal-graph [cbip]
    (reduce lg/add-edges
            (lg/graph)
            (for [c constraints
                  v1 (scope c)
                  v2 (scope c)
                  :when (not= v1 v2)]
              [v1 v2])))

  (triangulated-primal-graph [cbip ordering]
    (triangulate (primal-graph cbip) ordering))

  (factor-graph [cbip]
    (apply lg/graph (for [c constraints
                          v (scope c)]
                      [c v])))

  (junction-tree [cbip ordering]
    (-> cbip
        (triangulated-primal-graph ordering)
        (tree-decomposition ordering)))

  (eliminate-variable [cbip var]
    (let [grouped-constraints (group-by #(contains? (scope %) var) constraints)]
      (CBIProblem. semiring
                   (disj variables var)
                   (set (conj (get grouped-constraints false)
                              (sum-prod (get grouped-constraints true) var))))))

  (approx-eliminate-variable [this var b max-width]
    (let [new-constraints (approx-marginalize constraints var b max-width)]
      (CBIProblem. semiring
                   (disj variables var)
                   new-constraints)))

  (brute-inference [cbip vars]
    (let [svars (set vars)
          aux-vars (cs/difference variables svars)]
      (->constraint
       semiring
       svars
       (fn [m]
         ((reduce marginalize
                  (reduce combine
                          constraints)
                  aux-vars)
          m))))))

(defmethod print-method CBIProblem [cbip ^java.io.Writer w]
  (.write w "#Problem[")
  (print-method (constraints cbip) w)
  (.write w "]"))

(defn state-space [vars]
  (map (partial into {}) (apply combo/cartesian-product (map domain vars))))

(defn ->cbi-problem [constraints]
  (let [sr (sr (first constraints))
        vars (reduce cs/union #{} (map scope constraints))]
    (->CBIProblem sr vars constraints)))

(defn generalized-variable-elimination [cbi-problem elimination-ordering]
  (reduce eliminate-variable
          cbi-problem
          elimination-ordering))

(defn generalized-approximate-variable-elimination
  "Generalization of minibucket elimination"
  [cbi-problem elimination-ordering num-partitions max-width]
  (reduce #(approx-eliminate-variable %1 %2 num-partitions max-width)
          cbi-problem
          elimination-ordering))
