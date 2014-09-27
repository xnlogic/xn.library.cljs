(defproject xn.library.cljs "0.1.0-SNAPSHOT"
  :description "Library of simple pieces of code useful across our cljs projects and libraries"
  :url "http://xnlogic.com"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2342"]
                 [om "0.7.3"]
                 [com.andrewmcveigh/cljs-time "0.1.7-SNAPSHOT"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "xn.common.cljs"
              :source-paths ["src"]
              :compiler {
                :output-to "xn/common/cljs.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
