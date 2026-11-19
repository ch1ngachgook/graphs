(ns directed.travelling-salesman.route
  (:require [directed.arc :as a :refer [ends from to make-arc]]
            [directed.graph :as g]
            [directed.route :as r :refer [make-route]]
            [utils :refer [all-distinct?]]))

(defn hamiltonian-cycle? [graph route]
  (and (r/cycle? route)
       (r/graph-contains-route? graph route)
       (= (set (r/vertices route))
          (g/vertices graph))
       (all-distinct? (butlast (r/vertices route)))))

(defn closing-arc [route]
  (make-arc [(r/end route) (r/start route)]))

(defn close-route [route]
  (make-route (conj (r/vertices route) (r/start route))))

(defn connect-routes-by-arc
  "Присоединяет к дуге маршруты из данного набора,
  если конец маршрута совпадает с началом дуги,
  или если начало маршрута совпадает с концом дуги.
  Удаляет из набора найденные маршруты, добавляет в набор полученный новый маршрут.
  Возвращает полученный новый маршрут и обновлённый набор маршрутов."
  [arc routes]
  (let [before (first (filter #(= (r/end %) (from arc)) routes))
        after  (first (filter #(= (r/start %) (to arc)) routes))
        new-route (make-route (concat (butlast (r/vertices before))
                                      (ends arc)
                                      (rest (r/vertices after))))
        updated-routes (conj (disj routes before after) new-route)]
    [new-route updated-routes]))