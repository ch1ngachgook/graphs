(ns directed.networks.flow
  (:require [directed.networks.network :as n]
            [utils :refer [???]]))

(defn divergence
  "Возвращает дивергенцию функции в вершине сети"
  [network f vertex]
  (- (reduce + 0 (map f (n/outgoing-arcs network vertex)))
     (reduce + 0 (map f (n/incoming-arcs network vertex)))))

(defn flow?
  "Проверяет, что функция является потоком."
  [network f]
  (every? #(zero? (divergence network f %))
          (n/internal-vertices network)))

(defn circulation?
  "Проверяет, что функция является циркуляцией."
  [network f]
  (every? #(zero? (divergence network f %))
          (n/vertices network)))

(defn flow-value
  "Возвращает мощность потока"
  [network f]
  {:pre [(flow? network f)]}
  {:pre [(flow? network f)]}
  (divergence network f (n/source network)))

(defn zero-flow?
  "Проверяет, что поток является нулевым."
  [network f]
  {:pre [(flow? network f)]}
  {:pre [(flow? network f)]}
  (every? #(zero? (f %)) (n/arcs network)))

(defn positive-flow?
  "Проверяет, что поток является положительным."
  [network f]
  {:pre [(flow? network f)]}
  (let [values (map f (n/arcs network))]
    (and (every? #(>= % 0) values)
         (some pos? values))))

(defn add-flow
  "Возвращает сумму двух потоков"
  [f1 f2]
  (fn [arc] (+ (f1 arc) (f2 arc))))

(defn subtract-flow
  "Возвращает разность двух потоков"
  [f1 f2]
  (fn [arc] (- (f1 arc) (f2 arc))))
