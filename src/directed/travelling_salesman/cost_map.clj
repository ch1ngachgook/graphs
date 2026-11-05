(ns directed.travelling-salesman.cost-map
  (:require [directed.arc :refer [ends from to]]
            [utils :refer [???]]))

(defn reduce-costs-by
  "Выполняет один из этапов приведения матрицы (либо по строкам, либо по столбцам).
   Возвращает обновлённую матрицу и константу приведения."
  [cost-map selector-fn]
  (let [arcs (keys cost-map)
        min-costs (into {}
                        (map (fn [[v arcs]] [v (apply min (map cost-map arcs))])
                             (group-by selector-fn arcs)))
        new-cost-map (into {}
                           (map (fn [[arc cost]]
                                  [arc (- cost (min-costs (selector-fn arc)))])
                                cost-map))]
    [new-cost-map (apply + (vals min-costs))]))

(defn reduce-costs
  "Приводит матрицу стоимости. Возвращает обновлённую матрицу и константу приведения."
  [cost-map]
  (let [[cm-from min-costs-sum-from] (reduce-costs-by cost-map from)
        [cm-from-to min-costs-sum-to] (reduce-costs-by cm-from to)]
    [(???) (???)]))

(defn zero-cost-arcs
  "Возвращает набор дуг, соответствующих нулевым элементам в матрице стоимости."
  [cost-map]
  (???))

(defn arc-penalty
  "Вычисляет штраф для данной дуги."
  [cost-map arc]
  {:pre [(zero? (cost-map arc))]}
  (let [arcs (keys cost-map)
        min-cost-of-arcs (fn [f] (apply min (conj (map cost-map (filter f arcs)) ##Inf)))
        [i j] (ends arc)]
    (+ (min-cost-of-arcs #(and (= (from %) i) (not= (to %) j)))
       (min-cost-of-arcs #(and (not= (from %) i) (= (to %) j))))))

(defn highest-penalty-arc
  "Из всех дуг с нулевой стоимостью, возвращает дугу с максимальным штрафом."
  [cost-map]
  (???))

(defn eliminate-related-arcs
  "Удаляет из матрицы стоимости строку и столбец, соответствующий данной дуге.
  То есть удаляет дуги, у которых начало совпадает с началом данной дуги,
  и дуги, у которых конец совпадает с концом данной дуги."
  [cost-map arc]
  (???))
