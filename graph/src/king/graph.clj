(ns king.graph
  (:require [loom.graph :as lg]
            [loom.io :as li]
            [loom.alg :as la]
            [loom.label :as ll]
            [loom.attr :as l-attr]
            [plumbing.core :as pc]
            [clojure.set :refer [intersection
                                 union]]
            [taoensso.timbre :refer [spy debug]]
            )
  )


(defn shared-successors
  [g n1 n2]
  (intersection (lg/successors g n1)
                (lg/successors g n2)))


(defn moralize
  ([digraph]
   (moralize digraph
             (atom (lg/map->BasicEditableGraph
                    digraph))
             (:nodeset digraph) ))
  ([digraph egraph nodes-to-elim]

   (if (> (count nodes-to-elim) 1)
     (let [n1 (first nodes-to-elim)]
       (doseq [n2 (rest nodes-to-elim)]
         (when (not-empty (shared-successors digraph n1 n2))
           (swap! egraph lg/add-edges* [[n1 n2]]) ))
       (recur digraph egraph (rest nodes-to-elim)))
     @egraph
     )))

(defn neighbors
  [g n]
  (into #{} (keep identity (map
                            (fn [[x y]]
                              (cond
                                (= x n) y
                                (= y n) x
                                :else nil))
                            (la/distinct-edges g)))))


(defn triangulate
  ([g ordering]
   (let [adjs (pc/for-map [n ordering]
                       n
                       (neighbors g n)
                       )]
     (triangulate g ordering #{} adjs 0)))
  ([g ordering new-edges adjs w]
   (let [cur-var (first ordering)
         cur-neighbors (adjs cur-var)
         w (max w (count cur-neighbors))
         cur-edges (into #{} (union new-edges (lg/edges g)))
         non-chordal-neighbors (for [x cur-neighbors
                                     y cur-neighbors
                                     :when (and (not (contains? cur-edges [x y]))
                                                (not= x y))
                                     ]
                                 [x y])

         new-edges (reduce conj new-edges non-chordal-neighbors)
         adjs (reduce (fn [a [x y]]
                        (-> a
                            (update-in [x] conj y)
                            (update-in [y] conj x)))
                      adjs
                      non-chordal-neighbors)
         adjs (pc/map-vals #(disj % cur-var) adjs)
         ]
     (if (not-empty (rest ordering))
       (recur g (rest ordering) new-edges adjs w)
       {:graph (apply lg/graph (union (lg/edges g) new-edges))
        :width w})
     )))

(defn clique-tree
  [digraph ordering]
  (let [cliques (-> digraph
                    moralize
                    (triangulate ordering)
                    :graph
                    la/maximal-cliques)
        ;; two cliques have an edge if they share any nodes
        ;; we weight them by - | overlapping-nodes | so
        ;; we can get the maximum spanning tree using prims
        c-edges (for [c1 cliques
                      c2 cliques
                      :when (and (not= c1 c2)
                                 (not-empty (intersection c1 c2)))]
                  [c1 c2 (- (count (intersection c1 c2)))])
        c-graph (reduce
                 (fn add-edge [g e]
                   (lg/add-edges g e))
                 (lg/weighted-graph)
                 c-edges)
        mst (la/prim-mst c-graph)]
    (reduce (fn [g [n1 n2 _]]
              (ll/add-labeled-edges g [n1 n2] (intersection n1 n2)))
            (lg/graph)
            (lg/edges mst))))
