(ns directed.travelling-salesman.core-test
  (:require [clojure.test :refer :all]
            [directed.route :refer [arcs route]]
            [directed.weighted-graph :refer [route-length weighted-graph]]
            [vertices :refer :all])
  (:use [directed.travelling-salesman.core]))

;; @formatter:off
(def example-graph
  (weighted-graph [1 2 3 4 5]
                          [1 2 6] [1 3 9] [1 4 3] [1 5 9]
                  [2 1 3]         [2 3 6] [2 4 4] [2 5 2]
                  [3 1 3] [3 2 7]                 [3 5 4]
                  [4 1 1]         [4 3 3]         [4 5 2]
                  [5 1 2] [5 2 2] [5 3 6] [5 4 1]        ))
;; @formatter:on

(deftest shortest-hamiltonian-cycle-little-2-test
  (let [g (weighted-graph [A B] [A B 3] [B A 5])
        p (shortest-hamiltonian-cycle-little g)]
    (is (= 8 (route-length g p)))))

(deftest shortest-hamiltonian-cycle-little-3-test
  (let [g (weighted-graph [A B C]
                          [A B 2] [B C 3] [C A 4]
                          [B A 1] [C B 2] [A C 3])
        p (shortest-hamiltonian-cycle-little g)]
    (is (= 6 (route-length g p)))))

(deftest shortest-hamiltonian-cycle-little-3-one-path-test
  (let [g (weighted-graph [A B C] [A B 2] [B C 3] [C A 4])
        p (shortest-hamiltonian-cycle-little g)]
    (is (= 9 (route-length g p)))))

(deftest shortest-hamiltonian-cycle-little-3-same-cost-test
  (let [g (weighted-graph [A B C]
                          [A B 1] [A C 1]
                          [B A 1] [B C 1]
                          [C A 1] [C B 1])
        p (shortest-hamiltonian-cycle-little g)]
    (is (= 3 (route-length g p)))))

(deftest shortest-hamiltonian-cycle-little-4-same-cost-test
  (let [g (weighted-graph [A B C D]
                          [A B 1] [A C 1] [A D 1]
                          [B A 1] [B C 1] 
                          [C A 1] [C B 1] [C D 1]
                          [D A 1] [D B 1] [D C 1])
        p (shortest-hamiltonian-cycle-little g)]
    (is (= 4 (route-length g p)))))

(deftest shortest-hamiltonian-cycle-little-example-test
  (let [p (shortest-hamiltonian-cycle-little example-graph)]
    (testing "Длина цикла"
      (is (= 15 (route-length example-graph p))))
    (testing "Дуги цикла"
      (is (or (= (set (arcs (route 1 4 3 5 2 1)))
                 (set (arcs p)))
              (= (set (arcs (route 1 2 5 4 3 1)))
                 (set (arcs p))))))))
