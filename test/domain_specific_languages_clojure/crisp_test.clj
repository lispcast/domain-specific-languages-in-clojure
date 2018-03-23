(ns domain-specific-languages-clojure.crisp-test
  (:require [domain-specific-languages-clojure.crisp :as sut]
            [clojure.test :as t :refer :all]))

(deftest self-evaling
  (is (= "hello" (sut/crisp-eval {} "hello")))
  (is (= 123 (sut/crisp-eval {} 123)))
  (is (= nil (sut/crisp-eval {} nil))))

(deftest symbol-eval-lookup
  (is (= 1 (sut/crisp-eval {'x 1} 'x)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} 'x))))

(deftest special-forms
  (is (= 1 (sut/crisp-eval {} '(if true 1 2))))
  (is (= 2 (sut/crisp-eval {} '(if false 1 2))))

  (is (= 'x (sut/crisp-eval {} '(quote x))))
  (is (= 1 (sut/crisp-eval {} '(do 5 4 3 2 1))))
  
  (is (= 1 (sut/crisp-eval {} '(let [x 1] x))))
  (is (= 2 (sut/crisp-eval {} '(let [x 1 y 2] y))))
  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} '(let [x] y)))))
