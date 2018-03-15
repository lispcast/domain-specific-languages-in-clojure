(ns domain-specific-languages-clojure.core-test
  (:require [domain-specific-languages-clojure.core :as sut]
            [clojure.test :as t :refer :all]
            [clojure.walk :refer [macroexpand-all]]))

(deftest normalize-tests
  (is (= [:div {}] (sut/normalize [:div {}])))
  (is (= [:div {}] (sut/normalize [:div])))
  (is (= [:div {} "hello"] (sut/normalize [:div "hello"])))
  (is (= [:div {} "hello"] (sut/normalize [:div {} "hello"]))))

(deftest nil-empty-string
  (is (= (sut/unescaped "") (sut/eval-hiccup nil)))
  (is (= (sut/unescaped "") (sut/compile-hiccup nil)))
  (is (= (sut/unescaped "") (macroexpand-all `(sut/compile-hiccup nil)))))

(deftest string-itself
  (is (= (sut/unescaped "Hello, World!") (sut/eval-hiccup "Hello, World!")))
  (is (= (sut/unescaped "Hello, World!") (sut/compile-hiccup "Hello, World!")))
  (is (= (sut/unescaped "Hello, World!")
        (macroexpand-all
          `(sut/compile-hiccup "Hello, World!")))))

(deftest string-itself-html
  (is (= (sut/unescaped "Hello, World! &amp;nbsp;")
        (sut/eval-hiccup "Hello, World! &nbsp;")))
  (is (= (sut/unescaped "Hello, World! &amp;nbsp;")
        (sut/compile-hiccup "Hello, World! &nbsp;")))
  (is (= (sut/unescaped "Hello, World! &amp;nbsp;")
        (macroexpand-all
          `(sut/compile-hiccup "Hello, World! &nbsp;")))))

(deftest number-string
  (is (= (sut/unescaped "123") (sut/eval-hiccup 123)))
  (is (= (sut/unescaped "123") (sut/compile-hiccup 123)))
  (is (= (sut/unescaped "123")
        (macroexpand-all
          `(sut/compile-hiccup 123)))))

(deftest vector-empty
  (is (= (sut/unescaped "") (sut/eval-hiccup [])))
  (is (= (sut/unescaped "") (sut/compile-hiccup [])))
  (is (= (sut/unescaped "")
        (macroexpand-all `(sut/compile-hiccup [])))))

(deftest vector-div-html
  (is (= (sut/unescaped "<div></div>") (sut/eval-hiccup [:div])))
  (is (= (sut/unescaped "<div></div>") (sut/compile-hiccup [:div]))))

(deftest vector-span-html
  (is (= (sut/unescaped "<span></span>") (sut/eval-hiccup [:span])))
  (is (= (sut/unescaped "<span></span>") (sut/compile-hiccup [:span]))))

(deftest vector-attr-html
  (is (= (sut/unescaped "<div class=\"abc\"></div>")
        (sut/eval-hiccup [:div {:class "abc"}])))
  (is (= (sut/unescaped "<div class=\"abc\"></div>")
        (sut/compile-hiccup [:div {:class "abc"}]))))

(deftest vector-children-html
  (is (= (sut/unescaped "<div>abc</div>")
        (sut/eval-hiccup [:div {} "abc"])))
  (is (= (sut/unescaped "<div>abc</div>")
        (sut/compile-hiccup [:div {} "abc"]))))

(deftest vector-children-no-attr-html
  (is (= (sut/unescaped "<div>abc</div>") (sut/eval-hiccup [:div "abc"])))
  (is (= (sut/unescaped "<div>abc</div>") (sut/compile-hiccup [:div "abc"]))))

(deftest vector-if-html
  (is (= (sut/unescaped "<div>abc</div>")
        (sut/eval-hiccup (if true [:div {} "abc"]))))
  (is (= (sut/unescaped "<div>abc</div>")
        (sut/compile-hiccup (if true [:div {} "abc"])))))

(deftest vector-if-nested-html
  (is (= (sut/unescaped "<div><span>abc</span></div>")
        (sut/eval-hiccup [:div {} (if true [:span (if true "abc" "def")] [:div])])))
  (is (= (sut/unescaped "<div><span>abc</span></div>")
        (sut/compile-hiccup [:div {} (if true [:span (if true "abc" "def")] [:div])]))))

(deftest vector-children-if-html
  (is (= (sut/unescaped "<div>abc</div>")
        (sut/eval-hiccup [:div {} (if true "abc" "efg")])))
  (is (= (sut/unescaped "<div>abc</div>")
        (sut/compile-hiccup [:div {} (if true "abc" "efg")]))))

(deftest vector-children-2-children-html
  (is (= (sut/unescaped "<div>abcdef</div>")
        (sut/eval-hiccup [:div "abc" "def"])))
  (is (= (sut/unescaped "<div>abcdef</div>")
        (sut/compile-hiccup [:div "abc" "def"]))))

(deftest vector-div-div-html
  (is (= (sut/unescaped "<div><div></div></div>")
        (sut/eval-hiccup [:div [:div]])))
  (is (= (sut/unescaped "<div><div></div></div>")
        (sut/compile-hiccup [:div [:div]]))))

(deftest vector-div-eval-div-html
  (is (= (sut/unescaped "<div><div></div></div>")
        (sut/eval-hiccup [:div (sut/eval-hiccup [:div])])))
  (is (= (sut/unescaped "<div><div></div></div>")
        (sut/compile-hiccup [:div (sut/eval-hiccup [:div])]))))

(deftest idempotence
  (is (= (sut/unescaped "abc") (sut/eval-hiccup (sut/eval-hiccup "abc"))))
  (is (= (sut/unescaped "abc") (sut/compile-hiccup (sut/compile-hiccup "abc")))))

(deftest vector-div-div-br-escape-html
  (is (= (sut/unescaped "<div><div>&lt;br&gt;</div></div>")
        (sut/eval-hiccup [:div [:div "<br>"]])))
  (is (= (sut/unescaped "<div><div>&lt;br&gt;</div></div>")
        (sut/compile-hiccup [:div [:div "<br>"]]))))

(deftest vector-list-html
  (is (= (sut/unescaped "<div><div></div>hello<span>abc</span></div>")
        (sut/eval-hiccup [:div (list [:div] "hello" [:span "abc"])])))
  (is (= (sut/unescaped "<div><div></div>hello<span>abc</span></div>")
        (sut/compile-hiccup [:div (list [:div] "hello" [:span "abc"])]))))

(deftest unknown-error
  (is (thrown? clojure.lang.ExceptionInfo
        (sut/eval-hiccup [:div {} "abc" {}])))
  (is (thrown? clojure.lang.ExceptionInfo
        (sut/compile-hiccup [:div {} "abc" {}]))))
