(ns directed.travelling-salesman.route-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc]]
            [directed.graph :refer [graph]]
            [directed.route :refer [route]]
            [vertices :refer :all])
  (:use [directed.travelling-salesman.route]))

(deftest hamiltonian-cycle?-test
  (let [g (graph [A B C D] [A B] [B C] [A D] [C A] [C D] [D A])]
    (is (hamiltonian-cycle? g (route A B C D A))
        "Гамильтонов цикл")
    (is (hamiltonian-cycle? g (route B C D A B))
        "Ещё один гамильтонов цикл")
    (is (not (hamiltonian-cycle? g (route A B C D)))
        "Маршрут должен быть циклическим")
    (is (not (hamiltonian-cycle? g (route A B C D E F A)))
        "Все вершины маршрута должны быть вершинами графа")
    (is (not (hamiltonian-cycle? g (route A C B D A)))
        "Все дуги маршрута должны быть дугами графа")
    (is (not (hamiltonian-cycle? g (route A B C A)))
        "Маршрут должен содержать все вершины графа")
    (is (not (hamiltonian-cycle? g (route A B C D A B C D A)))
        "Все дуги маршрута должны быть различны")
    (is (not (hamiltonian-cycle? g (route A B C A D A)))
        "Маршрут должен проходить через каждую промежуточную вершину один раз")))

(deftest closing-arc-test
  (testing "Маршрут из двух вершин"
    (is (= (arc B A)
           (closing-arc (route A B)))))
  (testing "Маршрут из четырёх вершин"
    (is (= (arc D A)
           (closing-arc (route A B C D))))))

(deftest close-route-test
  (testing "Маршрут из двух вершин"
    (is (= (route A B A)
           (close-route (route A B)))))
  (testing "Маршрут из четырёх вершин"
    (is (= (route A B C D A)
           (close-route (route A B C D))))))

(deftest connect-routes-by-arc-test
  (testing "Пустое множество маршрутов"
    (let [[new-route updated-routes] (connect-routes-by-arc (arc C D) #{})]
      (is (= (route C D) new-route))
      (is (= #{(route C D)} updated-routes))))
  (testing "Подходящих маршрутов нет"
    (let [[new-route updated-routes] (connect-routes-by-arc (arc C D) #{(route A B) (route E F G)})]
      (is (= (route C D) new-route))
      (is (= #{(route C D) (route A B) (route E F G)} updated-routes))))
  (testing "Добавлен маршрут перед дугой"
    (let [[new-route updated-routes] (connect-routes-by-arc (arc C D) #{(route A B C) (route E F G)})]
      (is (= (route A B C D) new-route))
      (is (= #{(route A B C D) (route E F G)} updated-routes))))
  (testing "Добавлен маршрут после дуги"
    (let [[new-route updated-routes] (connect-routes-by-arc (arc C D) #{(route A B) (route D E F G)})]
      (is (= (route C D E F G) new-route))
      (is (= #{(route C D E F G) (route A B)} updated-routes))))
  (testing "Добавлены маршруты до и после дуги"
    (let [[new-route updated-routes] (connect-routes-by-arc (arc C D) #{(route A B C) (route D E F) (route G H)})]
      (is (= (route A B C D E F) new-route))
      (is (= #{(route A B C D E F) (route G H)} updated-routes)))))
