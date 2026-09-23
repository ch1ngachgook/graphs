(ns undirected.connected
  (:require [clojure.set :as set]
            [undirected.edge :as e]
            [undirected.graph :refer :all]
            [utils :refer [???]]))

(defn connected-component [graph start]
  (loop [visited #{}
         remaining #{start}]
    (if (empty? remaining)
      (make-graph visited
                  (filter #(set/subset? (e/ends %) visited)
                          (edges graph)))
      (let [adjacent (set (mapcat (fn [v] (adjacent-vertices graph v))
                                  remaining))
            unvisited (set/difference adjacent visited)]
        (recur (set/union visited remaining)
               unvisited)))))

(defn connected-components [graph]
  {:post [(mutually-disjoint? %)]}
  (loop [components []
         remaining (vertices graph)]
    (if (empty? remaining)
      components
      (let [v (first remaining)
            comp (connected-component graph v)]
        (recur (conj components comp)
               (set/difference remaining (vertices comp)))))))

(defn connected-vertices? [graph, v1, v2]
  (contains? (vertices (connected-component graph v1)) v2))

(defn connected? [graph]
  (<= (count (connected-components graph)) 1))
