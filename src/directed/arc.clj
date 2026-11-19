(ns directed.arc)

(defn arc [a b]
  (assert (not= a b) "Дуга должна содержать две разные вершины")
  {:start a, :end b})

(defn make-arc [[a b]]
  (arc a b))

(defn start [arc]
  (:start arc))

(def from start)

(defn end [arc]
  (:end arc))

(def to end)

(defn ends [arc]
  [(start arc) (end arc)])

(defn arc-fn
  "Создаёт функцию, которая принимает заданные значения на данных дугах,
  а в остальных случаях возвращает 0.
  Дуги заданы в виде троек (начало дуги, конец дуги, значение функции на этой дуге)."
  [& arc-values]
  (let [value-map (into {} (map (fn [[from to value]] [(arc from to) value])
                                arc-values))]
    (fn [a] (get value-map a 0))))
