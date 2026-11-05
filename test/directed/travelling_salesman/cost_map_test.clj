(ns directed.travelling-salesman.cost-map-test
  (:require [clojure.test :refer :all]
            [directed.arc :refer [arc from to]]
            [vertices :refer :all])
  (:use [directed.travelling-salesman.cost-map]))

;; @formatter:off
(deftest reduce-costs-by-from-test
  (let [cm1 {          (arc A B) 5,    (arc A C) 7,
             (arc B A) 4,              (arc B C) 2,
             (arc C A) 6, (arc C B) 1             }
        [cm2 min-costs-sum] (reduce-costs-by cm1 from)]
    (is (= {             (arc A B) 0, (arc A C) 2,
            (arc B A) 2,              (arc B C) 0,
            (arc C A) 5, (arc C B) 0             }
           cm2))
    (is (= (+ 5 2 1)
           min-costs-sum))))

(deftest reduce-costs-by-to-test
  (let [cm1 {             (arc A B) 0, (arc A C) 2,
             (arc B A) 2,              (arc B C) 0,
             (arc C A) 5, (arc C B) 0             }
        [cm2 min-costs-sum] (reduce-costs-by cm1 to)]
    (is (= {             (arc A B) 0, (arc A C) 2,
            (arc B A) 0,              (arc B C) 0,
            (arc C A) 3, (arc C B) 0             }
           cm2))
    (is (= (+ 2 0 0)
           min-costs-sum))))
;; @formatter:on

;; @formatter:off
(deftest reduce-costs-test
  (let [cm1 {             (arc A B) 5, (arc A C) 7,
             (arc B A) 4,              (arc B C) 2,
             (arc C A) 6, (arc C B) 1             }
        [cm2 h] (reduce-costs cm1)]
    (is (= {             (arc A B) 0, (arc A C) 2,
            (arc B A) 0,              (arc B C) 0,
            (arc C A) 3, (arc C B) 0             }
           cm2))
    (is (= (+ 5 2 1 2)
           h))))
;; @formatter:on

;; @formatter:off
(deftest zero-cost-arcs-test
  (let [cost-map {             (arc A B) 0, (arc A C) 1,
                  (arc B A) 0,              (arc B C) 0,
                  (arc C A) 2, (arc C B) 0              }]
    (is (= #{(arc A B) (arc B A) (arc B C) (arc C B)}
           (set (zero-cost-arcs cost-map))))))
;; @formatter:on

;; @formatter:off
(deftest arc-penalty-test
  (let [cost-map {             (arc A B) 0, (arc A C) 1,
                  (arc B A) 0,              (arc B C) 0,
                  (arc C A) 2, (arc C B) 0              }]
    (is (= 1 (arc-penalty cost-map (arc A B))) "A B")
    (is (= 2 (arc-penalty cost-map (arc B A))) "B A")
    (is (= 1 (arc-penalty cost-map (arc B C))) "B C")
    (is (= 2 (arc-penalty cost-map (arc C B))) "C B")))
;; @formatter:on

;; @formatter:off
(deftest arc-penalty-infinity-test
  (let [cost-map {             (arc A B) 0,
                  (arc B A) 0,              (arc B C) 0,
                  (arc C A) 2, (arc C B) 0              }]
    (is (= ##Inf (arc-penalty cost-map (arc A B))) "A B")
    (is (= 2 (arc-penalty cost-map (arc B A))) "B A")
    (is (= ##Inf (arc-penalty cost-map (arc B C))) "B C")
    (is (= 2 (arc-penalty cost-map (arc C B))) "C B")))
;; @formatter:on

;; @formatter:off
(deftest highest-penalty-arc-test
  (let [cost-map {             (arc A B) 0, (arc A C) 1,
                  (arc B A) 0,              (arc B C) 0,
                  (arc C A) 2, (arc C B) 0              }
        a (highest-penalty-arc cost-map)]
    (is (contains? #{(arc B A) (arc C B)} a))))
;; @formatter:on

;; @formatter:off
(deftest eliminate-related-arcs-test
  (let [cm1 {             (arc A B) 0, (arc A C) 1,
             (arc B A) 0,              (arc B C) 0,
             (arc C A) 2, (arc C B) 0             }
        cm2 {(arc A B) 0, (arc A C) 1,
             (arc C B) 0             }]
    (is (= cm2 (eliminate-related-arcs cm1 (arc B A))))))
;; @formatter:on
