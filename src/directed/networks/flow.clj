(ns directed.networks.flow
  (:require [directed.networks.network :as n]
            [utils :refer [???]]))

(defn divergence
  "Возвращает дивергенцию функции в вершине сети"
  [network f vertex]
  (???))

(defn flow?
  "Проверяет, что функция является потоком."
  [network f]
  (???))

(defn circulation?
  "Проверяет, что функция является циркуляцией."
  [network f]
  (???))

(defn flow-value
  "Возвращает мощность потока"
  [network f]
  {:pre [(flow? network f)]}
  (???))

(defn zero-flow?
  "Проверяет, что поток является нулевым."
  [network f]
  {:pre [(flow? network f)]}
  (???))

(defn positive-flow?
  "Проверяет, что поток является положительным."
  [network f]
  {:pre [(flow? network f)]}
  (???))

(defn add-flow
  "Возвращает сумму двух потоков"
  [f1 f2]
  (???))

(defn subtract-flow
  "Возвращает разность двух потоков"
  [f1 f2]
  (???))
