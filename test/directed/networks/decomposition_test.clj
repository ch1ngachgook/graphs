(ns directed.networks.decomposition-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc arc-fn]]
            [directed.networks.network :as n :refer [network]]
            [directed.networks.flow :refer [flow? circulation? flow-value add-flow]]
            [directed.route :refer [route]]
            [vertices :refer :all])
  (:use [directed.networks.decomposition]))

(deftest elementary-path?-test
  (let [net (network S T [S A B C T] [S A] [S B] [A B] [B C] [B S] [B T] [C A] [C T] [T B])]
    (is (elementary-path? net (route S A B C T)) "(S, T)-путь")
    (is (elementary-path? net (route T B S)) "(T, S)-путь")
    (is (elementary-path? net (route S B S)) "Цикл, проходящий через источник")
    (is (elementary-path? net (route B C T B)) "Цикл, проходящий через сток")
    (is (elementary-path? net (route A B C A)) "Цикл, проходящий через внутренние вершины")
    (is (not (elementary-path? net (route S A B C))) "Не содержит сток и не цикл")
    (is (not (elementary-path? net (route B C T))) "Не содержит источник и не цикл")
    (is (not (elementary-path? net (route A B C))) "Не содержит ни источник ни сток и не цикл")
    (is (thrown? AssertionError (elementary-path? net (route S A C T))) "В графе нет ребра (A, C)")))

(deftest elementary-path-flow-test
  (let [net (network S T [S A B C T] [S A] [S B] [A B] [B C] [B S] [B T] [C A] [C T] [T B])]
    (testing "Элементарный поток вдоль (S, T)-пути"
      (let [path (route S A B C T)
            f (elementary-path-flow 15 path)]
        (is (elementary-path? net path) "Элементарный путь")
        (is (flow? net f) "Элементарный поток действительно является потоком")
        (is (= 15 (flow-value net f)) "Мощность потока")
        (is (= 15 (f (arc S A))) "Дуга (S, A) есть в маршруте")
        (is (= 15 (f (arc A B))) "Дуга (A, B) есть в маршруте")
        (is (= 15 (f (arc B C))) "Дуга (B, C) есть в маршруте")
        (is (= 15 (f (arc C T))) "Дуга (C, T) есть в маршруте")
        (is (= 0 (f (arc S B))) "Дуги (S, B) нет в маршруте")
        (is (= 0 (f (arc A C))) "Дуги (A, C) нет в маршруте")
        (is (= 0 (f (arc B T))) "Дуги (B, T) нет в маршруте")))
    (testing "Элементарный поток вдоль (T, S)-пути"
      (let [path (route T B S)
            f (elementary-path-flow 25 path)]
        (is (elementary-path? net path) "Элементарный путь")
        (is (flow? net f) "Элементарный поток действительно является потоком")
        (is (= -25 (flow-value net f)) "Мощность потока")
        (is (= 25 (f (arc T B))) "Дуга (T, B) есть в маршруте")
        (is (= 25 (f (arc B S))) "Дуга (B, S) есть в маршруте")
        (is (= 0 (f (arc S B))) "Дуги (S, B) нет в маршруте")
        (is (= 0 (f (arc B T))) "Дуги (B, T) нет в маршруте")))
    (testing "Элементарный поток вдоль цикла"
      (let [cycle (route A B C A)
            f (elementary-path-flow 38 cycle)]
        (is (elementary-path? net cycle) "Элементарный путь")
        (is (flow? net f) "Элементарный поток действительно является потоком")
        (is (circulation? net f) "Элементарный поток на цикле является циркуляцией")
        (is (= 0 (flow-value net f)) "Мощность потока")
        (is (= 38 (f (arc A B))) "Дуга (A, B) есть в маршруте")
        (is (= 38 (f (arc B C))) "Дуга (B, C) есть в маршруте")
        (is (= 38 (f (arc C A))) "Дуга (C, A) есть в маршруте")
        (is (= 0 (f (arc S A))) "Дуги (S, A) нет в маршруте")
        (is (= 0 (f (arc B T))) "Дуги (B, T) нет в маршруте")))))

(deftest find-positive-arc-test
  (testing "(S, T)-путь"
    (let [net (network S T [S A B T] [S A] [S T] [A B] [A T] [B A] [B T])
          f (arc-fn [S A 25] [A B 35] [B A 10] [B T 25])]
      (is (flow? net f) "Поток")
      (is (= (arc S A) (find-positive-arc net f)) "Дуга")))
  (testing "Цикл"
    (let [net (network S T [S A B T] [S A] [S T] [A B] [A T] [B A] [B T])
          f (arc-fn [A B 10] [B A 10])]
      (is (flow? net f) "Поток")
      (is (contains? #{(arc A B) (arc B A)} (find-positive-arc net f)) "Дуга")))
  (testing "(T, S)-путь"
    (let [net (network S T [S A B T] [A S] [B A] [T B] [T A])
          f (arc-fn [T A 8] [T B 5] [B A 5] [A S 13])]
      (is (flow? net f) "Поток")
      (is (= (arc T A) (find-positive-arc net f)) "Дуга"))))

(deftest find-positive-elementary-path-test
  (testing "(S, T)-путь"
    (let [net (network S T [S A B T] [S A] [S T] [A B] [A T] [B A] [B T])
          f (arc-fn [S A 25] [A B 35] [B A 10] [B T 25])]
      (is (flow? net f) "Поток")
      (is (= (route S A B T) (find-positive-elementary-path net f)) "Элементарный путь")))
  (testing "Цикл"
    (let [net (network S T [S A B T] [S A] [S T] [A B] [A T] [B A] [B T])
          f (arc-fn [A B 10] [B A 10])]
      (is (flow? net f) "Поток")
      (is (= (route A B A) (find-positive-elementary-path net f)) "Элементарный путь")))
  (testing "(T, S)-путь"
    (let [net (network S T [S A B T] [A S] [B A] [T B] [T A])
          f (arc-fn [T A 8] [T B 5] [B A 5] [A S 13])]
      (is (flow? net f) "Поток")
      (is (= (route T A S) (find-positive-elementary-path net f)) "Элементарный путь"))))

(deftest decompose-flow-test
  (let [net (network S T
                     [S A B C D T]
                     [S A] [S C]
                     [A B] [A C]
                     [B C] [B T]
                     [C D]
                     [D B] [D T]
                     [T S])
        common-flow-map {(arc S A) 9, (arc S C) 6,
                         (arc A B) 4, (arc A C) 5,
                         (arc B C) 3, (arc B T) 4,
                         (arc C D) 14,
                         (arc D B) 3, (arc D T) 11}
        check-flow (fn [flow]
                     (let [elements (decompose-flow net flow)
                           elementary-flows (map (fn [[path value]] (elementary-path-flow value path))
                                                 elements)
                           combined-flow (reduce add-flow elementary-flows)]
                       (is (every? #(elementary-path? net %)
                                   (map second elements))
                           "Все пути являются элементарными")
                       (is (every? (fn [a] (= (flow a) (combined-flow a)))
                                   (n/arcs net))
                           "Значения на всех дугах совпадают")))]
    (testing "Поток является циркуляцией"
      (let [flow-map (assoc common-flow-map (arc T S) 15)
            flow (fn [a] (get flow-map a 0))]
        (is (circulation? net flow) "Циркуляция")
        (check-flow flow)))
    (testing "Поток не является циркуляцией"
      (let [flow-map (assoc common-flow-map (arc T S) 10)
            flow (fn [a] (get flow-map a 0))]
        (is (flow? net flow) "Поток")
        (check-flow flow)))
    (testing "Поток без дуги (T, S)"
      (let [flow (fn [a] (get common-flow-map a 0))]
        (is (flow? net flow) "Поток")
        (check-flow flow)))))

(deftest decompose-flow-backward-test
  (let [net (network S T [S A B T] [S A] [A S] [B A] [T B] [T A])
        f (arc-fn [T A 8] [T B 5] [B A 5] [A S 13])
        elements (decompose-flow net f)]
    (is (= #{[(route T B A S) 5] [(route T A S) 8]} (set elements)) "Элементарные пути")))
