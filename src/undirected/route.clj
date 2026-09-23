(ns undirected.route
  (:require [utils :refer [consecutive-pairs subsequence? all-distinct? ???]]
            [undirected.edge :refer [make-edge]]
            [undirected.graph :as g]))

(defn make-route [vertices]
  {:vertices (vec vertices)
   :edges    (vec (map make-edge (consecutive-pairs vertices)))})

(defn route [& vertices]
  (make-route vertices))

(defn vertices [route]
  (:vertices route))

(defn edges [route]
  (:edges route))

(defn length [route]
  (count (edges route)))

(defn start [route]
  (first (vertices route)))

(defn end [route]
  (last (vertices route)))

(defn contains-vertex? [route vertex]
  (boolean (some #(= vertex %) (vertices route))))

(defn add-vertex [route vertex]
  (make-route (conj (vertices route) vertex)))

(defn subroute? [subroute route]
  (subsequence? (vertices sub) (vertices route)))

(defn route->graph [route]
  (g/make-graph (set (vertices route))
                (set (edges route))))

(defn graph-contains-route? [graph route]
  (g/subgraph? (route->graph route) graph))

(defn routes-same-graph? [r1 r2]
  (= (route->graph r1) (route->graph r2)))

(defn chain? [route]
  (all-distinct? (edges route)))

(defn- simple? [route]
  (or (all-distinct? (vertices route))
      (and (= (start route) (end route))
           (all-distinct? (rest (vertices route))))))

(defn simple-chain? [route]
  (and (simple? route)
       (chain? route)))

(defn cyclic? [route]
  (= (start route) (end route)))

(defn cycle? [route]
  (and (cyclic? route)
       (chain? route)))

(defn simple-cycle? [route]
  (and (cyclic? route)
       (simple-chain? route)))

(defn- extract-simple-chain-from [vertices]
  (loop [chain []
         remaining vertices]
    (if (empty? remaining)
      chain
      (let [v (first remaining)
            chain-without-v (if (some #{v} chain)
                              (vec (take (inc (.indexOf chain v)) chain))
                              chain)]
        (recur (conj chain-without-v v)
               (rest remaining))))))

(defn extract-simple-chain [route]
  {:pre  [(not (cyclic? route))]
   :post [(subroute? % route) (simple-chain? %)]}
  (make-route (extract-simple-chain-from (vertices route))))

(defn extract-simple-cycle [route]
  {:pre  [(cyclic? route)]
   :post [(subroute? % route) (simple-cycle? %)]}
  (let [vs (vertices route)]
    (make-route (cons (start route)
                      (extract-simple-chain-from (rest vs))))))

(defn find-simple-cycle [graph]
  {:pre  [(every? (fn [d] (>= d 2)) (g/degrees graph))]
   :post [(graph-contains-route? graph %) (simple-cycle? %)]}
  (let [v0 (first (g/vertices graph))
        v1 (first (g/adjacent-vertices graph v0))]
    (loop [passed [v0]
           last v0
           current v1]
      (if (some #{current} passed)
        (make-route (conj (subvec passed (.indexOf passed current)) current))
        (recur (conj passed current)
               current
               (first (disj (g/adjacent-vertices graph current) last)))))))
