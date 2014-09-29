(defproject xn.library.cljs "0.1.0"
  :description "Library of simple pieces of code useful across our cljs projects and libraries"
  :url "http://xnlogic.com"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2356"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [om "0.7.3"]
                 [com.andrewmcveigh/cljs-time "0.2.1"]
                 [com.cognitect/transit-cljs "0.8.188"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "xn.library.cljs"
              :source-paths ["src"]
              :compiler {
                :output-to "target/xn.library.cljs.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
