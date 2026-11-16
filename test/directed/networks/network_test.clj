(ns directed.networks.network-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc]]
            [directed.graph :refer [graph]]
            [vertices :refer :all])
  (:use [directed.networks.network]))

(deftest make-network-test
  (let [net (network S T [S A B C T] [S A] [A B] [B C] [C T])]
    (is (= S (source net)) "Источник")
    (is (= T (target net)) "Сток")
    (is (= #{A B C} (internal-vertices net)) "Внутренние вершины")
    (is (= #{A B C S T} (vertices net)) "Вершины")
    (is (= #{(arc S A) (arc A B) (arc B C) (arc C T)} (arcs net)) "Дуги")))

(deftest terminal?-test
  (let [net (network S T [S A B C T] [S A] [A B] [B C] [C T])]
    (is (terminal? net S) "Источник")
    (is (terminal? net T) "Сток")
    (is (not (terminal? net A)) "Внутренняя вершина A ")
    (is (not (terminal? net B)) "Внутренняя вершина B")
    (is (thrown? AssertionError (terminal? net D)) "В графе нет вершины D")))

(deftest incoming-arcs-test
  (let [g (graph [A B C D] [A B] [B C] [B D] [C D])]
    (is (= #{} (incoming-arcs g A)) "Вершина A")
    (is (= #{(arc A B)} (incoming-arcs g B)) "Вершина B")
    (is (= #{(arc B C)} (incoming-arcs g C)) "Вершина C")
    (is (= #{(arc B D) (arc C D)} (incoming-arcs g D)) "Вершина D")))

(deftest outgoing-arcs-test
  (let [g (graph [A B C D] [A B] [B C] [B D] [C D])]
    (is (= #{(arc A B)} (outgoing-arcs g A)) "Вершина A")
    (is (= #{(arc B C) (arc B D)} (outgoing-arcs g B)) "Вершина B")
    (is (= #{(arc C D)} (outgoing-arcs g C)) "Вершина C")
    (is (= #{} (outgoing-arcs g D)) "Вершина D")))
