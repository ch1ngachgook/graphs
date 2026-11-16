(ns directed.arc-test
  (:require [clojure.test :refer [deftest is]]
            [vertices :refer :all])
  (:use directed.arc))

(deftest arc-test
  (let [a (arc A B)]
    (is (= A (start a)))
    (is (= B (end a)))
    (is (= [A B] (ends a))))
  (is (thrown? AssertionError (arc 1 1))))

(deftest make-arc-test
  (is (= (arc C D) (make-arc [C D]))))

(deftest arc-fn-test
  (let [f (arc-fn [A B 10] [C D -15])]
    (is (= 10 (f (arc A B))) "Дуга (A, B)")
    (is (= -15 (f (arc C D))) "Дуга (C, D)")
    (is (= 0 (f (arc E F))) "Дуга (E, F) не задана")))
