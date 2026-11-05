(ns directed.travelling-salesman.node-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc]]
            [directed.route :refer [route]]
            [directed.weighted-graph :refer [weighted-graph]]
            [directed.travelling-salesman.core-test :refer [example-graph]]
            [vertices :refer :all])
  (:use [directed.travelling-salesman.node]))

(deftest all-cycles-node-test
  (let [g (weighted-graph [A B C] [A B 3] [B C 5] [C A 7])
        n (all-cycles-node g)]
    (is (= {(arc A B) 0, (arc B C) 0, (arc C A) 0}
           (cost-map n)))
    (is (= 15
           (lower-bound n)))
    (is (= #{}
           (path-fragments n)))))

;; @formatter:off
(deftest make-node-reduce-costs-test
  (let [n (make-node {             (arc A B) 5, (arc A C) 7,
                      (arc B A) 4,              (arc B C) 2,
                      (arc C A) 6, (arc C B) 1             }
                     0
                     #{(route D E)})]
    (is (= {             (arc A B) 0, (arc A C) 2,
            (arc B A) 0,              (arc B C) 0,
            (arc C A) 3, (arc C B) 0             }
           (cost-map n)))
    (is (= (+ 5 2 1 2)
           (lower-bound n)))
    (is (= #{(route D E)}
           (path-fragments n)))))
;; @formatter:on

;; @formatter:off
(deftest make-node-reduce-costs-example-test
  (let [n (all-cycles-node example-graph)]
    (is (= {             (arc 1 2) 2, (arc 1 3) 4, (arc 1 4) 0, (arc 1 5) 6,
            (arc 2 1) 1,              (arc 2 3) 2, (arc 2 4) 2, (arc 2 5) 0,
            (arc 3 1) 0, (arc 3 2) 3,                           (arc 3 5) 1,
            (arc 4 1) 0,              (arc 4 3) 0,              (arc 4 5) 1,
            (arc 5 1) 1, (arc 5 2) 0, (arc 5 3) 3, (arc 5 4) 0             }
           (cost-map n)))
    (is (= (+ 3 2 3 1 1
              0 1 2 0 0)
           (lower-bound n)))
    (is (= #{}
           (path-fragments n)))))
;; @formatter:on

;; @formatter:off
(deftest cycles-without-arc-node-test
    (let [n1 (make-node {             (arc A B) 0, (arc A C) 1,
                        (arc B A) 0,              (arc B C) 0,
                        (arc C A) 2, (arc C B) 0             }
                       10
                       #{(route D E)})
          n2 (cycles-without-arc-node n1 (arc B A))]
      (is (= {             (arc A B) 0, (arc A C) 1,
                                        (arc B C) 0,
              (arc C A) 0, (arc C B) 0             }
             (cost-map n2)))
      (is (= 12
             (lower-bound n2)))
      (is (= #{(route D E)}
             (path-fragments n2)))))
;; @formatter:on

;; @formatter:off
(deftest cycles-with-arc-node-test
    (let [n1 (make-node {             (arc A B) 0, (arc A C) 1,
                         (arc B A) 0,              (arc B C) 0,
                         (arc C A) 2, (arc C B) 0             }
                        10
                        #{})
          n2 (cycles-with-arc-node n1 (arc B A))]
      (is (= {            (arc A C) 0,
              (arc C B) 0            }
             (cost-map n2)))
      (is (= 11
             (lower-bound n2)))
      (is (= #{(route B A)}
             (path-fragments n2)))))
;; @formatter:on

(deftest cycles-with-arc-node-example-test
  (let [n1 (make-node {(arc 2 1) 0 (arc 2 3) 1,
                       (arc 3 1) 0 (arc 3 5) 0,
                       (arc 4 3) 0 (arc 4 5) 0}
                      15
                      #{(route 1 4) (route 5 2)})
        n2 (cycles-with-arc-node n1 (arc 2 1))]
    (is (= 15
           (lower-bound n2)))
    (is (= {(arc 3 5) 0,
            (arc 4 3) 0}
           (cost-map n2)))
    (is (= #{(route 5 2 1 4)}
           (path-fragments n2)))))

;; @formatter:off
(deftest branch-on-highest-penalty-arc-test
  (let [n (make-node {             (arc A B) 0, (arc A C) 1,
                      (arc B A) 0,              (arc B C) 0,
                      (arc C A) 2, (arc C B) 0             }
                      10
                      #{(route D E)})
        [n-without n-with] (branch-on-highest-penalty-arc n)]
    (testing "Не включающие дугу"
      (is (= {             (arc A B) 0, (arc A C) 1,
              (arc B A) 0,              (arc B C) 0,
              (arc C A) 0,                         }
             (cost-map n-without)))
      (is (= 12
             (lower-bound n-without)))
      (is (= #{(route D E)}
             (path-fragments n-without))))
    (testing "Включающие дугу"
      (is (= {             (arc A C) 0,
              (arc B A) 0             }
             (cost-map n-with)))
      (is (= 11
             (lower-bound n-with)))
      (is (= #{(route D E) (route C B)}
             (path-fragments n-with))))))
;; @formatter:on

;; @formatter:off
(deftest branch-on-highest-penalty-arc-infinity-test
  (let [n (make-node {                          (arc A C) 1,
                      (arc B A) 0,              (arc B C) 0,
                      (arc C A) 2, (arc C B) 0             }
                     10
                     #{(route D E)})
        [n-with] (branch-on-highest-penalty-arc n)]
    (testing "Включающие дугу"
      (is (= {             (arc A C) 0,
              (arc B A) 0             }
             (cost-map n-with)))
      (is (= 11
             (lower-bound n-with)))
      (is (= #{(route D E) (route C B)}
             (path-fragments n-with))))))
;; @formatter:on

(deftest leaf?-test
  (testing "Узел не является листом"
    (let [node (make-node {(arc A B) 0, (arc A C) 1,
                           (arc B C) 0,
                           (arc C A) 2, (arc C B) 0}
                          42
                          #{(arc D E)})]
      (is (not (leaf? node)))))
  (testing "Узел является листом"
    (let [node (make-node {(arc B C) 1, (arc D A) 2}
                          15
                          #{(route A B) (route C D)})]
      (is (leaf? node)))))

(deftest get-cycle-test
  (let [n (make-node {(arc B C) 1, (arc D A) 2}
                     15
                     #{(route A B) (route C D)})
        p (get-cycle n)]
    (is (= (route A B C D A) p))))
