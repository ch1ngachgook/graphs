(ns directed.networks.flow-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc-fn]]
            [directed.networks.network :as n :refer [network]]
            [vertices :refer :all])
  (:use [directed.networks.flow]))

(deftest divergence-test
  (let [net (network A D [A B C D] [A B] [B C] [B D] [C D])]
    (testing "Постоянная функция"
      (let [f (constantly 10)]
        (is (= 10 (divergence net f A)) "Вершина A")
        (is (= 10 (divergence net f B)) "Вершина B")
        (is (= 0 (divergence net f C)) "Вершина C")
        (is (= -20 (divergence net f D)) "Вершина D")
        (is (= 0 (reduce + (map #(divergence net f %) (n/vertices net)))) "Сумма дивергенций")))
    (testing "Функция, отличная от нуля на одной дуге"
      (let [f (arc-fn [B C 15])]
        (is (= 0 (divergence net f A)) "Вершина A")
        (is (= 15 (divergence net f B)) "Вершина B")
        (is (= -15 (divergence net f C)) "Вершина C")
        (is (= 0 (divergence net f D)) "Вершина D")
        (is (= 0 (reduce + (map #(divergence net f %) (n/vertices net)))) "Сумма дивергенций")))))

(deftest flow?-test
  (let [net (network S T [S A B T] [S A] [S T] [A B] [A T] [B A] [B T])]
    (testing "Постоянная функция = 0 - поток"
      (let [f (constantly 0)]
        (is (flow? net f))))
    (testing "Постоянная функция > 0 - не поток"
      (let [f (constantly 10)]
        (is (not (flow? net f)))))
    (testing "Постоянная функция > 0 - поток на другой сети"
      (let [f (constantly 10)]
        (is (flow? (network S T [S A T] [S A] [A T]) f))))
    (testing "Функция, отличная от нуля на дуге (A B) - не поток"
      (let [f (arc-fn [A B 15])]
        (is (not (flow? net f)))))
    (testing "Функция, отличная от нуля на дуге (S T) из источника в сток - поток"
      (let [f (arc-fn [S T 20])]
        (is (flow? net f))))
    (testing "Функция, отличная от нуля на дугах (A B) и (B A) - поток"
      (let [f (arc-fn [A B 25] [B A 25])]
        (is (flow? net f))))))

(deftest circulation?-test
  (let [net (network A D [A B C D] [A B] [A D] [B C] [B D] [C B] [C D])]
    (testing "Постоянная функция = 0 - циркуляция"
      (let [f (constantly 0)]
        (is (circulation? net f))))
    (testing "Постоянная функция > 0 - не циркуляция"
      (let [f (constantly 10)]
        (is (not (circulation? net f)))))
    (testing "Функция, отличная от нуля на дуге (B C) - не циркуляция"
      (let [f (arc-fn [B C 15])]
        (is (not (circulation? net f)))))
    (testing "Функция, отличная от нуля на дуге (A D) из источника в сток - не циркуляция"
      (let [f (arc-fn [A D 20])]
        (is (not (circulation? net f)))))
    (testing "Функция, отличная от нуля на дугах (B C) и (C B) - циркуляция"
      (let [f (arc-fn [B C 25] [C B 25])]
        (is (circulation? net f))))))

(deftest flow-value-test
  (let [net (network S T [S A B T] [S A] [S T] [A B] [A T] [B A] [B T])]
    (testing "Функция, отличная от нуля на дуге (S, T) из источника в сток"
      (let [f (arc-fn [S T 20])]
        (is (= 20 (flow-value net f)))))
    (testing "Функция, отличная от нуля на дугах (A, B) и (B, A)"
      (let [f (arc-fn [A B 25] [B A 25])]
        (is (= 0 (flow-value net f)))))))

(deftest zero-flow?-test
  (let [net (network S T [S A B T] [S A] [A B] [B A] [B T])]
    (testing "Функция не является потоком"
      (let [f (constantly 10)]
        (is (not (flow? net f)) "Не поток")
        (is (thrown? AssertionError (zero-flow? net (constantly 10))))))
    (testing "Ненулевой поток"
      (let [f (arc-fn [S A 10] [A B 5] [B A -5] [B T 10])]
        (is (flow? net f) "Поток")
        (is (not (zero-flow? net f)) "не нулевой")))
    (testing "Нулевой поток"
      (let [f (constantly 0)]
        (is (flow? net f) "Поток")
        (is (zero-flow? net (constantly 0)) "нулевой")))
    (testing "Функция принимает ненулевое значение на дуге, не содержащейся в сети - нулевой поток"
      (let [f (arc-fn [S T 10])]
        (is (flow? net f) "Поток")
        (is (zero-flow? net (constantly 0)) "нулевой")))))

(deftest positive-flow?-test
  (let [net (network S T [S A B T] [S A] [S B] [S T] [A B] [B A] [B S] [B T] [T B])]
    (testing "Функция не является потоком"
      (let [f (constantly 10)]
        (is (not (flow? net f)) "Не поток")
        (is (thrown? AssertionError (positive-flow? net f)))))
    (testing "Нулевой поток"
      (let [f (constantly 0)]
        (is (flow? net f) "Поток")
        (is (not (positive-flow? net f)) "не положительный")))
    (testing "Поток с разными знаками"
      (let [f (arc-fn [S A -10] [A B -5] [B A 5] [B T -10])]
        (is (flow? net f) "Поток")
        (is (not (positive-flow? net f)) "не положительный")))
    (testing "Отрицательный поток"
      (let [f (arc-fn [S A -10] [A B -15] [B A -5] [B T -10])]
        (is (flow? net f) "Поток")
        (is (not (positive-flow? net f)) "не положительный")))
    (testing "Положительный поток"
      (let [f (arc-fn [S A 10] [A B 15] [B A 5] [B T 10])]
        (is (flow? net f) "Поток")
        (is (positive-flow? net f) "положительный")))
    (testing "Функция принимает отрицательное значение на дуге, не содержащейся в сети - положительный поток"
      (let [f (arc-fn [S T 10] [C D -15])]
        (is (flow? net f) "Поток")
        (is (positive-flow? net f) "положительный")))))

(deftest add-flow-test
  (let [f1 (fn [x] (inc x))
        f2 (fn [x] (* 2 x))
        f (add-flow f1 f2)]
    (is (= 1 (f 0)) 0)
    (is (= 4 (f 1)) 1)
    (is (= 7 (f 2)) 2)
    (is (= 10 (f 3)) 1)))

(deftest subtract-flow-test
  (let [f1 (fn [x] (inc x))
        f2 (fn [x] (* 2 x))
        f (subtract-flow f1 f2)]
    (is (= 1 (f 0)) 0)
    (is (= 0 (f 1)) 1)
    (is (= -1 (f 2)) 2)
    (is (= -2 (f 3)) 1)))
