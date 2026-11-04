(ns directed.shortest-path
  (:require [clojure.set :as set]
            [directed.graph :as g :refer [neighbours]]
            [directed.route :as r :refer [make-route]]
            [directed.weighted-graph :refer [arc-length]]
            [utils :refer [all-distinct? consecutive-pairs]]))

(defn- error [message]
  (throw (Exception. ^String message)))

(defn path-from-to? [graph start end route]
  (and (r/path? route)
       (= start (r/start route))
       (= end   (r/end route))
       (all-distinct? route)
       (r/graph-contains-route? graph route)))

(defn path-info [distance-from-start previous-vertex]
  {:dist distance-from-start, :prev previous-vertex})

(defn distance [path-map v]
  (get-in path-map [v :dist] ##Inf))

(defn previous [path-map v]
  (get-in path-map [v :prev]))

(defn update-distances [graph path-map v neighbours]
  (let [dv (distance path-map v)]
    (for [u neighbours
          :let [d (+ dv (arc-length graph v u))]
          :when (< d (distance path-map u))]
      [u (path-info d v)])))

(defn backtrack-path [start end path-map]
  (loop [vertices (list end)]
    (let [v (first vertices)]
      (if (= start v)
        (make-route vertices)
        (recur (cons (previous path-map v) vertices))))))

(defn shortest-path-dijkstra [graph start end]
  {:pre  [(g/contains-vertex? graph start) (g/contains-vertex? graph end)]
   :post [(path-from-to? graph start end %)]}
  (loop [path-map {start (path-info 0 nil)}
         unmarked-vs (set (g/vertices graph))]
    (if (every? (fn [u](= ##Inf (distance path-map u))) unmarked-vs)
      (error "There is no path from start to end")
      (let [marked-v (apply min-key (fn [u](distance path-map u)) unmarked-vs)
            next-unmarked-vs (disj unmarked-vs marked-v)]
        (if (= marked-v end)
          (backtrack-path start end path-map)
          (let [updates (update-distances graph path-map marked-v
                                          (filter next-unmarked-vs
                                                  (neighbours graph marked-v)))]
            (recur (into path-map updates)
                   next-unmarked-vs)))))))

(defn shortest-path-modified-dijkstra [graph start end]
  {:pre  [(g/contains-vertex? graph start) (g/contains-vertex? graph end)]
   :post [(path-from-to? graph start end %)]}
  (loop [path-map {start (path-info 0 nil)}
         unmarked-vs (set (g/vertices graph))]
    (if (every? (fn [u] (= ##Inf (distance path-map u))) unmarked-vs)
      (error "There is no path from start to end")
      (let [marked-v (apply min-key (fn [u] (distance path-map u)) unmarked-vs)
            updates (update-distances graph path-map marked-v
                                      (neighbours graph marked-v))
            updated-vs (set (map first updates))
            next-unmarked-vs (set/union (disj unmarked-vs marked-v) updated-vs)]
        (if (empty? next-unmarked-vs)
          (backtrack-path start end path-map)
          (recur (into path-map updates)
                 next-unmarked-vs))))))