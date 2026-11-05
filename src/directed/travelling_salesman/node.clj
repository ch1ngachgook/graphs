(ns directed.travelling-salesman.node
  (:require [directed.travelling-salesman.cost-map :as cm]
            [directed.travelling-salesman.route :as r]
            [directed.weighted-graph :as wg]
            [utils :refer [???]]))

(defn make-node
  "Создаёт узел, соответствующий множеству всех гамильтоновых циклов.
  Приводит полученную матрицу стоимостей, сохраняет приведённую матрицу.
  Добавляет константу приведения к нижней границе длины гамильтонова цикла."
  [cost-map lower-bound path-fragments]
  (let [[reduced-cost-map reduction-value] (cm/reduce-costs cost-map)]
    {:cost-map       reduced-cost-map
     :lower-bound    (+ lower-bound reduction-value)
     :path-fragments path-fragments}))

(defn cost-map
  "Возвращает матрицу приведения для данного узла."
  [node]
  (:cost-map node))

(defn lower-bound
  "Возвращает нижнюю границу длины гамильтонова цикла для данного узла."
  [node]
  (:lower-bound node))

(defn path-fragments
  "Возвращает множество путей, собранных из дуг, включённых в гамильтонов цикл."
  [node]
  (:path-fragments node))

(defn all-cycles-node
  "Создаёт узел, соответствующий множеству всех гамильтоновых циклов в данном графе."
  [graph]
  (make-node (???)
             (???)
             (???)))

(defn cycles-without-arc-node
  "Создаёт узел, соответствующий множеству гамильтоновых циклов, не включающих данную дугу."
  [node arc]
  (make-node (???)
             (lower-bound node)
             (path-fragments node)))

(defn cycles-with-arc-node
  "Создаёт узел, соответствующий множеству гамильтоновых циклов, включающих данную дугу."
  [node arc]
  (let [updated-cost-map (cm/eliminate-related-arcs (cost-map node) arc)
        [new-path updated-path-fragments] (r/connect-routes-by-arc arc (path-fragments node))
        closing-arc (r/closing-arc new-path)]
    (make-node (???)
               (lower-bound node)
               (???))))

(defn branch-on-highest-penalty-arc
  "Находит дугу с максимальным штрафом и разветвляет данный узел.
  Если штраф дуги меньше бесконечности, возвращает два узла:
    узел, соответствующий множеству гамильтоновых циклов, не включающих эту дугу,
    и узел, соответствующий множеству гамильтоновых циклов, включающих эту дугу.
  Если штраф дуги равен бесконечности, то возвращает только узел,
    соответствующий множеству гамильтоновых циклов, включающих эту дугу."
  [node]
  (let [arc (???)]
    (if (< (???) ##Inf)
      [(???) (???)]
      [(???)])))

(defn leaf?
  "Проверяет, что данный узел не требуется ветвить."
  [node]
  (???))

(defn get-cycle
  "Возвращает гамильтонов цикл, соответствующий данному узлу."
  [node]
  {:pre [(leaf? node)]}
  (let [arcs (keys (cost-map node))
        arc (first arcs)
        [path _] (r/connect-routes-by-arc arc (path-fragments node))]
    (assert (= (second arcs) (r/closing-arc path)))
    (???)))
