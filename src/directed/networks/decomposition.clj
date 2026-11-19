(ns directed.networks.decomposition
  (:require [clojure.set :as set]
            [directed.arc :as a]
            [directed.networks.network :as n]
            [directed.networks.flow :as f]
            [directed.route :as r :refer [make-route]]
            [utils :refer [???]]))

(defn elementary-path?
  "Проверяет, что маршрут является элементарным путём."
  [network route]
  {:pre [(r/graph-contains-route? network route)]}
  (???))

(defn elementary-path-flow
  "Возвращает элементарный поток для данного элементарного пути -
  функцию, которая на дугах пути принимает данное значение, а на остальных дугах равна нулю."
  [value path]
  (???))

(defn find-positive-arc
  "Находит в сети дугу, на которой поток принимает положительное значение.
  Начинает поиск с дуг, выходящих из полюсов."
  [network flow]
  {:post [(pos? (flow %))]}
  (let [terminal-arcs (set/union (n/outgoing-arcs network (n/source network))
                                 (n/outgoing-arcs network (n/target network)))
        positive-terminal-arcs (set/select #(pos? (flow %))
                                           terminal-arcs)]
    (if (not (empty? positive-terminal-arcs))
      (apply max-key flow positive-terminal-arcs)
      (apply max-key flow (n/arcs network)))))

(defn find-positive-elementary-path
  "Находит в сети элементарный путь, на всех дугах которого поток принимает положительное значение."
  [network flow]
  {:post [(elementary-path? network %)]}
  (let [starting-arc (find-positive-arc network flow)]
    (loop [vertices (a/ends starting-arc)]
      (let [current-v (peek vertices)
            out-arcs (set/select #(pos? (flow %))
                                 (n/outgoing-arcs network current-v))]
        (if (empty? out-arcs)
          (make-route vertices)
          (let [next-arc (apply max-key flow out-arcs)
                next-v (a/to next-arc)]
            (if (some #{next-v} vertices)
              (make-route (conj (vec (drop-while #(not= % next-v) vertices))
                                next-v))
              (recur (conj vertices next-v)))))))))

(defn decompose-flow
  "Раскладывает поток на элементарные потоки.
  Возвращает набор пар, каждая из которых содержит элементарный путь
  и константное значение элементарного потока на этом пути."
  [network flow]
  {:pre [(f/positive-flow? network flow)]}
  (loop [remaining-flow flow
         elements []]
    (if (???)
      elements
      (let [path (find-positive-elementary-path network remaining-flow)
            value (???)
            path-flow (???)]
        (assert (f/positive-flow? network path-flow))
        (recur (???)
               (???))))))
