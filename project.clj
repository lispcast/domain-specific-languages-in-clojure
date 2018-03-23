(defproject domain-specific-languages-clojure "0.1.0-SNAPSHOT"
  :description "Part of PurelyFunctional.tv"
  :url "https://purelyfunctional.tv/"
  :license {:name "CC0 1.0 Universal (CC0 1.0) Public Domain Dedication"
            :url "http://creativecommons.org/publicdomain/zero/1.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.apache.commons/commons-text "1.2"]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.22.0"]
                             [io.aviso/pretty "0.1.34"]]
                   :dependencies [[pjstadig/humane-test-output "0.8.3"]
                                  [io.aviso/pretty "0.1.34"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]}})
