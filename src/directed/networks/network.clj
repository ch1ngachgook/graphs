(ns directed.networks.network
  (:require [clojure.set :as set]
            [directed.arc :refer [from make-arc to]]
            [directed.graph :as g :refer [make-graph]]
            [utils :refer [???]]))

(defn make-network
  "Создаёт сеть с данными источником, стоком, вершинами и дугами."
  [source target vertices arcs]
  (let [graph (make-graph vertices arcs)]
    (assert (g/contains-vertex? graph source) "Источник является одной из вершин графа")
    (assert (g/contains-vertex? graph target) "Сток является одной из вершин графа")
    (assoc graph :source source
                 :target target)))

(defn network
  "Создаёт сеть с данными источником, стоком, вершинами и дугами.
  Дуги заданы в виде пар вершин (начало и конец дуги)."
  [source target vertices & arcs]
  (make-network source target vertices (map make-arc arcs)))

(def vertices g/vertices)

(def arcs g/arcs)

(defn source
  "Возвращает источник сети."
  [network]
  (:source network))

(defn target
  "Возвращает сток сети."
  [network]
  (:target network))

(defn terminal?
  "Проверяет, является ли вершина полюсом (либо источником, либо стоком)."
  [network vertex]
  {:pre [(g/contains-vertex? network vertex)]}
  (???))

(defn internal-vertices
  "Возвращает множество внутренних вершин сети."
  [network]
  (???))

(defn incoming-arcs
  "Возвращает множество дуг, входящих в вершину."
  [graph vertex]
  (???))

(defn outgoing-arcs
  "Возвращает множество дуг, выходящих из вершины."
  [graph vertex]
  (???))
