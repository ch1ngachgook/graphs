(ns directed.all-paths
  (:require [directed.arc :refer [end ends]]
            [directed.graph :as g :refer [arcs]]
            [directed.route :refer [make-route]]
            [directed.weighted-graph :refer [weights]]
            [utils :refer [???]]))

(defn path-info [value step]
  {:value value
   :step  step})

(defn value [path-map i j]
  (:value (path-map [i j])))

(defn step [path-map i j]
  (:step (path-map [i j])))

(defn initial-path-map [graph]
  (into {} (for [[a w] (weights graph)]
             (let [[i j] (ends a)]
               [[i j] (path-info w j)]))))

(defn- print-path-map [graph path-map]
  (let [vertices (sort (g/vertices graph))]
    (doseq [i vertices]
      (doseq [j vertices]
        (print (format "%5s" (or (value path-map i j) "nil"))))
      (println)))
  (println))

(defn update-path-length [graph path-map p]
  ;(print-path-map graph path-map)
  (let [distance (fn [i j] (or (value path-map i j) ##Inf))]
    (for [i (g/vertices graph)
          j (g/vertices graph)
          :let [d (+ (distance i p) (distance p j))]
          :when (< d (distance i j))]
      [[i j] (path-info d (step path-map i p))])))

(defn update-path-width [graph path-map p]
  ;(print-path-map graph path-map)
  (let [min-width (fn [i j] (or (value path-map i j) ##-Inf))]
    (for [i (g/vertices graph)
          j (g/vertices graph)
          :let [m (min (min-width i p) (min-width p j))]
          :when (> m (min-width i j))]
      [[i j] (path-info m (step path-map i p))])))

(defn collect-path [path-map start end]
  (if (not (contains? path-map [start end]))
    nil
    (loop [vertices [start]]
      (let [v (peek vertices)]
        (if (and (= v end)
                 (not (and (= start end) (= vertices [start]))))
          (make-route vertices)
          (recur (conj vertices (step path-map v end))))))))

(defn collect-all-paths [graph path-map]
  (into {} (for [i (g/vertices graph)
                 j (g/vertices graph)]
             [[i j] (collect-path path-map i j)])))

(defn- all-paths-floyd [graph update-path-map]
  (let [final-path-map (reduce (fn [path-map p]
                                 (into path-map (update-path-map graph path-map p)))
                               (initial-path-map graph)
                               (g/vertices graph))]
    (collect-all-paths graph final-path-map)))

(defn all-shortest-paths-floyd [graph]
  (all-paths-floyd graph update-path-length))

(defn all-widest-paths-floyd [graph]
  (all-paths-floyd graph update-path-width))

(defn route-width [graph route]
  (reduce min (map (fn [a] ((weights graph) a)) (arcs route))))