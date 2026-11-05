(ns directed.travelling-salesman.route
  (:require [directed.arc :as a :refer [ends from to]]
            [directed.graph :as g]
            [directed.route :as r :refer [make-route]]
            [utils :refer [???]]))

(defn hamiltonian-cycle?
  "Проверяет, что маршрут является гамильтоновым циклом в данном графе"
  [graph route]
  (???))

(defn closing-arc
  "Возвращает дугу, соединяющую конец и начало маршрута
  (добавление такой дуги к маршруту делает его циклическим)."
  [route]
  (???))

(defn close-route
  "Замыкает маршрут, делает его циклическим"
  [route]
  (???))

(defn connect-routes-by-arc
  "Присоединяет к дуге маршруты из данного набора,
  если конец маршрута совпадает с началом дуги,
  или если начало маршрута совпадает с концом дуги.
  Удаляет из набора найденные маршруты, добавляет в набор полученный новый маршрут.
  Возвращает полученный новый маршрут и обновлённый набор маршрутов."
  [arc routes]
  (let [before (first (filter #(= (r/end %) (from arc)) routes))
        after (first (filter #(= (r/start %) (to arc)) routes))
        new-route (make-route (concat (butlast (r/vertices before))
                                      (ends arc)
                                      (rest (r/vertices after))))
        updated-routes (conj (disj routes before after) new-route)]
    [new-route updated-routes]))
