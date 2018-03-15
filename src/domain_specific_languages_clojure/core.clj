(ns domain-specific-languages-clojure.core
  (:require [clojure.string :as str])
  (:import [org.apache.commons.text StringEscapeUtils]))

(deftype EscapedString [s]
  Object
  (toString [this]
    s)
  (equals [this other]
    (if (instance? EscapedString other)
      (= (.-s this) (.-s other))
      false)))

(defn unescaped [s]
  (cond
    (instance? EscapedString s)
    s

    (string? s)
    (EscapedString. s)

    :else
    (throw (ex-info "I don't know how to unescaped this value." {:value s}))))

(defn escape [s]
  (cond
    (instance? EscapedString s)
    s

    (string? s)
    (unescaped (StringEscapeUtils/escapeHtml4 s))

    :else
    (throw (ex-info "I don't know how to escape this value." {:value s}))))

(defn normalize [hiccup]
  (case (count hiccup)
    1 (let [[tag] hiccup]
        [tag {}])
    2 (let [[tag attr?] hiccup]
        (if (map? attr?)
          [tag attr?]
          [tag {} attr?]))
    (let [[tag attr? & children] hiccup
          attr (if (map? attr?)
                 attr?
                 {})
          children (if (map? attr?)
                     children
                     (cons attr? children))]
      (into [tag attr] children))))

;; hiccup -> EscapedString (HTML)
(defn eval-hiccup [hiccup]
  (cond
    (nil? hiccup)
    (unescaped "")

    (string? hiccup)
    (unescaped (escape hiccup))

    (instance? EscapedString hiccup)
    hiccup

    (number? hiccup)
    (unescaped (str hiccup))

    (vector? hiccup)
    (if (empty? hiccup)
      (unescaped "")
      (let [[tag attr & children] (normalize hiccup)]
        (unescaped
          (str "<" (name tag)
            (str/join
              (for [[k v] attr]
                (str " " (name k) "=\"" v \")))
            ">"

            (str/join (map eval-hiccup children))

            "</" (name tag) ">"))))

    (seq? hiccup)
    (str/join (map eval-hiccup hiccup))

    :else
    (throw (ex-info "I don't know how to handle this data as hiccup." {:hiccup hiccup}))))

(defmacro compile-hiccup [hiccup]
  (cond
    (nil? hiccup)
    (unescaped "")

    (string? hiccup)
    (escape hiccup)

    (instance? EscapedString hiccup)
    hiccup

    (number? hiccup)
    (unescaped (str hiccup))

    (vector? hiccup)
    (if (empty? hiccup)
      (unescaped "")
      (let [[tag attr & children] (normalize hiccup)]
        `(unescaped
           (str "<" ~(name tag)
             ~@(for [[k v] attr]
                 (str " " (name k) "=\"" v \"))
             ">"

             ~@(for [child children]
                 `(compile-hiccup ~child))

             "</" ~(name tag) ">"))))

    (seq? hiccup)
    (let [[op & args] hiccup]
      (case op
        if (let [[test then else] args]
             `(if ~test
                (compile-hiccup ~then)
                (compile-hiccup ~else)))
        `(eval-hiccup ~hiccup)))

    :else
    `(eval-hiccup ~hiccup)))

(defn comment-component [unsanitized-text]
  (eval-hiccup
    [:div {:class "comment"}
     unsanitized-text]))
