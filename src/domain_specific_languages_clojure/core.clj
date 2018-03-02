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
  (let [[tag attr? & children] hiccup
        attr (if (map? attr?)
               attr?
               {})
        children (if (map? attr?)
                   children
                   (cons attr? children))]
    (into [tag attr] children)))

;; hiccup -> EscapedString (HTML)
(defn eval-hiccup [hiccup]
  (cond
    (nil? hiccup)
    (unescaped "")

    (string? hiccup)
    (unescaped (escape hiccup))

    (number? hiccup)
    (unescaped (str hiccup))

    (vector? hiccup)
    (let [[tag attr & children] (normalize hiccup)]
      (unescaped
        (str "<" (name tag)
          (str/join
            (for [[k v] attr]
              (str " " (name k) "=\"" v \")))
          ">"

          (str/join (map eval-hiccup children))

          "</" (name tag) ">")))

    (seq? hiccup)
    (str/join (map eval-hiccup hiccup))

    :else
    (throw (ex-info "I don't know how to handle this data as hiccup." {:hiccup hiccup}))))

(defn comment-component [unsanitized-text]
  (eval-hiccup
    [:div {:class "comment"}
     unsanitized-text]))
