(ns undirected.graph
  (:require [clojure.set :as set]
            [undirected.edge :as e :refer [edge make-edge]]
            [utils :refer [all-distinct? pairs ???]]))

(defn make-graph [vertices edges]
  (assert (all-distinct? vertices), "Все вершины графа должны быть различны")
  (assert (all-distinct? edges), "Все рёбра графа должны быть различны")
  (let [vertex-set (set vertices)]
    (assert (every? (fn [e] (set/subset? (e/ends e) vertex-set)) edges)
            "Рёбра должны соединять вершины графа")
    {:vertices vertex-set
     :edges    (set edges)}))

(defn graph [vertices & vertex-pairs]
  (make-graph vertices
              (map make-edge vertex-pairs)))

(defn vertices [graph]
  (:vertices graph))

(defn edges [graph]
  (:edges graph))

(defn contains-vertex? [graph vertex]
  (contains? (vertices graph) vertex))

(defn contains-edge? [graph edge]
  (contains? (edges graph) edge))

(defn order [graph]
  (count (vertices graph)))

(defn edge-count [graph]
  (count (edges graph)))

(defn incident-edges [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (set (filter #(e/incident? % vertex)(edges graph))))

(defn adjacent-vertices [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (set(map #(e/other-end % vertex) (incident-edges graph vertex))))

(defn adjacent? [graph v1 v2]
  {:pre [(contains-vertex? graph v1) (contains-vertex? graph v2)]}
  (contains? (adjacent-vertices graph v1) v2))

(defn degree [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (count (incident-edges graph vertex)))

(defn degrees [graph]
  (map #(degree graph %) (vertices graph)))

(defn pendant? [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (= 1 (degree graph vertex)))

(defn isolated? [graph vertex]
  {:pre [(contains-vertex? graph vertex)]}
  (= 0 (degree graph vertex)))

(defn empty-graph? [graph]
    (empty? (edges graph)))

(defn empty-graph [vertices]
  (graph vertices))

(defn complete-graph? [graph]
  (let [n (order graph)]
      (= (edge-count graph) (quot (* n (dec n)) 2))))

(defn complete-graph [vertices]
  (apply graph vertices (pairs vertices))); Hint: use utils/pairs

(defn subgraph? [g1 g2]
  (???))

(defn union [g1 g2]
  (???))

(defn intersection [g1 g2]
  (???))

(defn difference [g1 g2]
  (???))

(defn disjoint? [g1 g2]
  (???))

(defn mutually-disjoint? [graphs]
  (???))  ; Hint: use utils/pairs
