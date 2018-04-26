(ns domain-specific-languages-clojure.crisp-test
  (:require [domain-specific-languages-clojure.crisp :as sut]
            [clojure.test :as t :refer :all]))

(deftest self-evaling
  (is (= "hello" (sut/crisp-eval {} "hello")))
  (is (= 123 (sut/crisp-eval {} 123)))
  (is (= nil (sut/crisp-eval {} nil)))

  (is (= "hello" ((sut/crisp-compile "hello") {})))
  (is (= 123 ((sut/crisp-compile 123) {})))
  (is (= nil ((sut/crisp-compile nil) {}))))

(deftest symbol-eval-lookup
  (is (= 1 (sut/crisp-eval {'x 1} 'x)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} 'x)))

  (is (= 1 ((sut/crisp-compile x) {'x 1})))
  (is (thrown? clojure.lang.ExceptionInfo
        ((sut/crisp-compile x) {}))))

(deftest special-forms
  (is (= 1 (sut/crisp-eval {} '(if true 1 2))))
  (is (= 2 (sut/crisp-eval {} '(if false 1 2))))

  (is (= 1 ((sut/crisp-compile (if true 1 2)) {})))
  (is (= 2 ((sut/crisp-compile (if false 1 2)) {})))

  
  (is (= 'x (sut/crisp-eval {} '(quote x))))
  (is (= 1 (sut/crisp-eval {} '(do 5 4 3 2 1))))

  (is (= 'x ((sut/crisp-compile (quote x)) {})))
  (is (= 1 ((sut/crisp-compile (do 5 4 3 2 1)) {})))


  
  (is (= 1 (sut/crisp-eval {} '(let [x 1] x))))
  (is (= 2 (sut/crisp-eval {} '(let [x 1 y 2] y))))
  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} '(let [x] y))))

  (is (= 1 ((sut/crisp-compile (let [x 1] x)) {})))
  (is (= 2 ((sut/crisp-compile (let [x 1 y 2] y)) {})))
  (is (thrown? clojure.lang.ExceptionInfo
        ((sut/crisp-compile (let [x] y)) {}))))

(deftest function-call
  (is (= + (sut/crisp-eval {'+ +} '+)))
  (is (= 10 (sut/crisp-eval {'+ +} '(+ 4 6))))
  (is (= + (sut/crisp-eval {} +)))


  (is (sut/crisp-eval {} '(fn [] 1)))
  (is (sut/crisp-fn? (sut/crisp-eval {} '(fn [] 1))))

  (is (sut/crisp-compile (fn [] 1)))
  (is (fn? ((sut/crisp-compile (fn [] 1)) {})))

  (let [f ((sut/crisp-compile (fn [a b c] c)) {})]
    (is (= 6 (f 4 5 6))))


  
  (is (= 1 (sut/crisp-eval {} '((fn [] 1)))))
  (is (= 2 (sut/crisp-eval {} '((fn [] 2)))))
  ;; environment
  (is (= 10 (sut/crisp-eval {'+ +}
              '(let [x 5]
                 ((fn [y]
                    (let [z 3]
                      (+ x y z)))
                  2)))))
  (is (not (sut/crisp-fn? 1)))

  (is (= 1 ((sut/crisp-compile ((fn [] 1)))
            {})))
  (is (= 2 ((sut/crisp-compile ((fn [] 2)))
            {})))
  ;; environment
  (is (= 10 ((sut/crisp-compile
               (let [x 5]
                 ((fn [y]
                    (let [z 3]
                      (+ x y z)))
                  2)))
             {'+ +})))

  

  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} '(1))))

  (is (= 7 (sut/crisp-eval {'+ +}
             '(let [x 5
                    f (fn [y] (+ x y))]
                (f 2)))))

  (is (= 1 (sut/crisp-eval {'x 1} '(let [y x] y))))

  (is (= 1 ((sut/crisp-compile (let [y x] y))
            {'x 1})))

  (is (thrown? clojure.lang.ExceptionInfo
        ((sut/crisp-compile (1)) {})))

  (is (= 7 ((sut/crisp-compile
              (let [x 5
                    f (fn [y] (+ x y))]
                (f 2)))
            {'+ +})))

  
  )
