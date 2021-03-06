(ns domain-specific-languages-clojure.crisp-test
  (:require [domain-specific-languages-clojure.crisp :as sut]
            [clojure.test :as t :refer :all]))

(deftest self-evaling
  (is (= "hello" (sut/crisp-eval {} "hello")))
  (is (= 123 (sut/crisp-eval {} 123)))
  (is (= nil (sut/crisp-eval {} nil)))

  (is (= "hello" ((sut/crisp-compile "hello" #{}) {})))
  (is (= 123 ((sut/crisp-compile 123 #{}) {})))
  (is (= nil ((sut/crisp-compile nil #{}) {}))))

(deftest symbol-eval-lookup
  (is (= 1 (sut/crisp-eval {'x 1} 'x)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} 'x)))

  (is (= 1 ((sut/crisp-compile x #{x}) {'x 1})))
  (is (thrown? clojure.lang.ExceptionInfo
        ((sut/crisp-compile x #{}) {}))))

(deftest special-forms
  (is (= 1 (sut/crisp-eval {} '(if true 1 2))))
  (is (= 2 (sut/crisp-eval {} '(if false 1 2))))

  (is (= 1 ((sut/crisp-compile (if true 1 2) #{}) {})))
  (is (= 2 ((sut/crisp-compile (if false 1 2) #{}) {})))


  (is (= 'x (sut/crisp-eval {} '(quote x))))
  (is (= 1 (sut/crisp-eval {} '(do 5 4 3 2 1))))

  (is (= 'x ((sut/crisp-compile (quote x) #{}) {})))
  (is (= 1 ((sut/crisp-compile (do 5 4 3 2 1) #{}) {})))



  (is (= 1 (sut/crisp-eval {} '(let [x 1] x))))
  (is (= 2 (sut/crisp-eval {} '(let [x 1 y 2] y))))
  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} '(let [x] y))))

  (is (= 1 ((sut/crisp-compile (let [x 1] x) #{}) {})))
  (is (= 2 ((sut/crisp-compile (let [x 1 y 2] y) #{}) {})))
  (is (thrown? clojure.lang.ExceptionInfo
        ((sut/crisp-compile (let [x] y) #{}) {}))))

(deftest function-call
  (is (= + (sut/crisp-eval {'+ +} '+)))
  (is (= 10 (sut/crisp-eval {'+ +} '(+ 4 6))))
  (is (= + (sut/crisp-eval {} +)))

  (is (= + ((sut/crisp-compile p #{p}) {'p +})))
  (is (= 10 ((sut/crisp-compile (+ 4 6) #{+}) {'+ +})))
  (is (= + ((sut/crisp-compile* + #{}) {})))


  (is (sut/crisp-eval {} '(fn [] 1)))
  (is (sut/crisp-fn? (sut/crisp-eval {} '(fn [] 1))))

  (is (sut/crisp-compile (fn [] 1) #{}))
  (is (fn? ((sut/crisp-compile (fn [] 1) #{}) {})))

  (let [f ((sut/crisp-compile (fn [a b c] c) #{}) {})]
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

  (is (= 1 ((sut/crisp-compile ((fn [] 1)) #{})
            {})))
  (is (= 2 ((sut/crisp-compile ((fn [] 2)) #{})
            {})))
  ;; environment
  (is (= 10 ((sut/crisp-compile
               (let [x 5]
                 ((fn [y]
                    (let [z 3]
                      (+ x y z)))
                  2))
               #{+})
             {'+ +})))



  (is (thrown? clojure.lang.ExceptionInfo (sut/crisp-eval {} '(1))))

  (is (= 7 (sut/crisp-eval {'+ +}
             '(let [x 5
                    f (fn [y] (+ x y))]
                (f 2)))))

  (is (= 1 (sut/crisp-eval {'x 1} '(let [y x] y))))

  (is (= 1 ((sut/crisp-compile (let [y x] y) #{x})
            {'x 1})))

  (is (thrown? clojure.lang.ExceptionInfo
        ((sut/crisp-compile (1) #{}) {})))

  (is (= 7 ((sut/crisp-compile
              (let [x 5
                    f (fn [y] (+ x y))]
                (f 2))
              #{+})
            {'+ +})))

  (is (thrown? clojure.lang.ExceptionInfo
        (sut/crisp-compile x #{})))

  (is (thrown? clojure.lang.ExceptionInfo
        (sut/crisp-compile (fn [a b c] a b d) #{})))

  (is (thrown? clojure.lang.ExceptionInfo
        (sut/crisp-compile (do d) #{})))

  )


(deftest instaparse-test
  (is (= [[:NUMBER "1"]] (sut/parse "1;")))
  (is (= [[:NUMBER "1.9"]] (sut/parse "1.9;")))
  (is (= [[:NUMBER ".9"]] (sut/parse ".9;")))
  (is (= [[:NUMBER "0.9"]] (sut/parse "0.9;")))

  (is (= [[:NUMBER "1"]] (sut/parse "     1;")))

  (is (= [[:STRING "abc"]] (sut/parse "\"abc\";")))
  (is (= [[:STRING ""]] (sut/parse "\"\";")))

  (is (= [[:IFTHENELSE
           [:NUMBER "1"]
           [:BLOCK [:NUMBER "2"]]
           [:BLOCK [:NUMBER "3"]]]]
        (sut/parse "if (1) { 2; } else { 3; }")))

  (is (= [[:IFTHENELSE
           [:NUMBER "1"]
           [:BLOCK [:NUMBER "1"] [:NUMBER "2"]]
           [:BLOCK [:NUMBER "4"] [:NUMBER "3"]]]]
        (sut/parse "if (1) { 1; 2; } else { 4; 3; }")))

  (is (= [[:IFTHENELSE
           [:NUMBER "1"]
           [:BLOCK [:NUMBER "1"] [:NUMBER "2"]]
           [:BLOCK [:NUMBER "4"] [:NUMBER "3"]]]]
        (sut/parse " if (1) { 1; 2; } else { 4; 3; }")))

  (is (= [[:IFTHENELSE
           [:NUMBER "1"]
           [:NUMBER "2"]
           [:NUMBER "3"]]]
        (sut/parse "

if (1)
  2;
else
  3;
")))

  (is (= [[:IFTHENELSE
           [:NUMBER "1"]
           [:FUNCTIONCALL [:VARIABLE "print"] [:ARGS [:NUMBER "2"]]]
           [:FUNCTIONCALL [:VARIABLE "print"] [:ARGS [:NUMBER "3"]]]]]
        (sut/parse "

if (1)
  print(2);
else
  print(3);
")))

  (is (= [[:VARIABLE "abc"]]
        (sut/parse "abc;")))

  (is (= [[:VARIABLEASSIGNMENT [:BINDINGS [:VARIABLE "abc"] [:NUMBER "2"]]
           [:BLOCK [:VARIABLE "abc"]]]]
        (sut/parse "let abc = 2 { abc; }")))

  (is (= [[:VARIABLEASSIGNMENT [:BINDINGS
                                [:VARIABLE "abc"] [:NUMBER "2"]
                                [:VARIABLE "xyz"] [:NUMBER "3"]]
           [:BLOCK [:VARIABLE "abc"]]]]
        (sut/parse "let abc = 2, xyz =3 { abc; }")))

  (is (= [[:FUNCTIONDEFINITION [:PARAMS]
           [:BLOCK [:VARIABLE "a"]]]]
        (sut/parse "function () { a; };")))

  (is (= [[:FUNCTIONDEFINITION [:PARAMS [:VARIABLE "a"]]
           [:BLOCK [:VARIABLE "a"]]]]
        (sut/parse "function (a ) { a; };")))

  (is (= [[:FUNCTIONDEFINITION [:PARAMS [:VARIABLE "a"] [:VARIABLE "b"]]
           [:BLOCK [:VARIABLE "a"]]]]
        (sut/parse "function (a, b ) { a; };")))

  (is (= [[:FUNCTIONCALL [:VARIABLE "fib"] [:ARGS [:NUMBER "1"]]]]
        (sut/parse "fib(1);")))

  (is (= [[:FUNCTIONCALL [:VARIABLE "fib"] [:ARGS]]]
        (sut/parse "fib(  );")))

  (is (= [[:FUNCTIONCALL [:VARIABLE "fib"] [:ARGS [:NUMBER "1"] [:VARIABLE "q"]]]]
        (sut/parse "fib( 1, q );")))
  )


(deftest rparse-test
  (is (= [nil nil] (sut/rparse "")))
  
  (is (= [1   nil] (sut/rparse "1")))
  (is (= [2   nil] (sut/rparse "2")))
  (is (= [2.0 nil] (sut/rparse "2.0")))

  (is (= ['abc  nil] (sut/rparse "abc")))
  (is (= ['abc? nil] (sut/rparse "abc?")))

  (is (= ["abc" nil] (sut/rparse "\"abc\"")))
  (is (= [""    nil] (sut/rparse "\"\"")))

  (is (= ['(1 2 3) nil] (sut/rparse "(1 2 3)")))

  (is (= ['(1 2 3) (seq "   ")] (sut/rparse "   (1 2 3)   ")))

  (is (= ['(1 2 3 (1)) nil] (sut/rparse "(1 2 3 (1))")))

  (is (= ['(1 2 3 (1 (1))) nil] (sut/rparse "(1 2 3 (1 (1)))")))

  (is (= ['((1)) nil] (sut/rparse "((1))")))

  (is (= ['(1 2 3 (1 2 3 (4) ((( 1))))) nil] (sut/rparse "(1 2 3 (1 2 3 (4) ((( 1)))))")))
  )
