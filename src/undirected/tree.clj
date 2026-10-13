(ns undirected.tree
  (:require [undirected.connected :refer [connected-components connected?]]
            [undirected.graph :as g]))

(defn edge-count-tree? [graph]
  (= (g/edge-count graph) (dec (g/order graph))))

(defn tree? [graph]
  (and (connected? graph)
       (edge-count-tree? graph)))

(defn forest? [graph]
  (let [n (g/order graph)
        m (g/edge-count graph)
        k (count (connected-components graph))]
    (= m (- n k))))