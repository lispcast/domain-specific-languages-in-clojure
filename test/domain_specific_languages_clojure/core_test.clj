(ns domain-specific-languages-clojure.core-test
  (:require [domain-specific-languages-clojure.core :as sut]
            [clojure.test :as t :refer :all]))

(deftest normalize-tests
  (is (= [:div {}] (sut/normalize [:div {}])))
  (is (= [:div {}] (sut/normalize [:div])))
  (is (= [:div {} "hello"] (sut/normalize [:div "hello"])))
  (is (= [:div {} "hello"] (sut/normalize [:div {} "hello"]))))

(deftest nil-empty-string
  (is (= (sut/unescaped "") (sut/eval-hiccup nil))))

(deftest string-itself
  (is (= (sut/unescaped "Hello, World!") (sut/eval-hiccup "Hello, World!"))))

(deftest string-itself-html
  (is (= (sut/unescaped "Hello, World! &amp;nbsp;") (sut/eval-hiccup "Hello, World! &nbsp;"))))

(deftest number-string
  (is (= (sut/unescaped "123") (sut/eval-hiccup 123))))

(deftest vector-empty
  (is (= (sut/unescaped "") (sut/eval-hiccup []))))

(deftest vector-div-html
  (is (= (sut/unescaped "<div></div>") (sut/eval-hiccup [:div]))))

(deftest vector-span-html
  (is (= (sut/unescaped "<span></span>") (sut/eval-hiccup [:span]))))

(deftest vector-attr-html
  (is (= (sut/unescaped "<div class=\"abc\"></div>") (sut/eval-hiccup [:div {:class "abc"}]))))

(deftest vector-children-html
  (is (= (sut/unescaped "<div>abc</div>") (sut/eval-hiccup [:div {} "abc"]))))

(deftest vector-children-no-attr-html
  (is (= (sut/unescaped "<div>abc</div>") (sut/eval-hiccup [:div "abc"]))))

(deftest vector-children-2-children-html
  (is (= (sut/unescaped "<div>abcdef</div>") (sut/eval-hiccup [:div "abc" "def"]))))

(deftest vector-div-div-html
  (is (= (sut/unescaped "<div><div></div></div>") (sut/eval-hiccup [:div [:div]]))))

(deftest vector-div-eval-div-html
  (is (= (sut/unescaped "<div><div></div></div>") (sut/eval-hiccup [:div (sut/eval-hiccup [:div])]))))

(deftest vector-div-div-br-escape-html
  (is (= (sut/unescaped "<div><div>&lt;br&gt;</div></div>") (sut/eval-hiccup [:div [:div "<br>"]]))))

(deftest vector-list-html
  (is (= (sut/unescaped "<div><div></div>hello<span>abc</span></div>") (sut/eval-hiccup [:div (list [:div] "hello" [:span "abc"])]))))

(deftest unknown-error
  (is (thrown? clojure.lang.ExceptionInfo
        (sut/eval-hiccup [:div {} "abc" {}]))))
