(ns domain-specific-languages-clojure.core
  (:require [clojure.string :as str]))

(defn normalize [hiccup]
  (let [[tag attr? & children] hiccup
          attr (if (map? attr?)
                 attr?
                 {})
          children (if (map? attr?)
                     children
                     (cons attr? children))]
    (into [tag attr] children)))

;; hiccup -> String (HTML)
(defn eval-hiccup [hiccup]
  (cond
    (nil? hiccup)
    ""
    
    (string? hiccup)
    hiccup
    
    (number? hiccup)
    (str hiccup)
    
    (vector? hiccup)
    (let [[tag attr & children] (normalize hiccup)]
      (str "<" (name tag)
        (str/join
          (for [[k v] attr]
            (str " " (name k) "=\"" v \")))
        ">"

        (str/join (map eval-hiccup children))

        "</" (name tag) ">"))

    (seq? hiccup)
    (str/join (map eval-hiccup hiccup))
    
    :else
    (throw (ex-info "I don't know how to handle this data as hiccup." {:hiccup hiccup}))))
