(ns domain-specific-languages-clojure.core-test
  (:require [domain-specific-languages-clojure.core :as sut]
            [clojure.test :as t :refer :all]))

(deftest nil-empty-string
  (is (= "" (sut/eval-hiccup nil))))

(deftest string-itself
  (is (= "Hello, World!" (sut/eval-hiccup "Hello, World!"))))

(deftest number-string
  (is (= "123" (sut/eval-hiccup 123))))

(deftest vector-div-html
  (is (= "<div></div>" (sut/eval-hiccup [:div]))))

(deftest vector-span-html
  (is (= "<span></span>" (sut/eval-hiccup [:span]))))

(deftest vector-attr-html
  (is (= "<div class=\"abc\"></div>" (sut/eval-hiccup [:div {:class "abc"}]))))

(deftest vector-children-html
  (is (= "<div>abc</div>" (sut/eval-hiccup [:div {} "abc"]))))

(deftest vector-children-no-attr-html
  (is (= "<div>abc</div>" (sut/eval-hiccup [:div "abc"]))))

(deftest vector-children-2-children-html
  (is (= "<div>abcdef</div>" (sut/eval-hiccup [:div "abc" "def"]))))

(deftest vector-div-div-html
  (is (= "<div><div></div></div>" (sut/eval-hiccup [:div [:div]]))))

(deftest vector-list-html
  (is (= "<div><div></div>hello<span>abc</span></div>" (sut/eval-hiccup [:div (list [:div] "hello" [:span "abc"])]))))

(deftest unknown-error
  (is (thrown? clojure.lang.ExceptionInfo
        (sut/eval-hiccup [:div {} "abc" {}]))))
