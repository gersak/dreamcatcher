(defproject org.roribib/dreamcatcher "1.0.2"
  :description "Dreamcatcher is a realy small library that
               strives to simulate state machine behavior."
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [com.taoensso/timbre "2.0.0"]
                 [clj-time "0.5.0"]
                 [hiccup "1.0.3"]
                 [prismatic/dommy "0.1.1"]]
  :plugins [[lein-cljsbuild "0.3.0"]
            [lein-clojars "0.9.1"]]
  :source-paths ["src" "src-cljs"]
  :profiles {:dev {:source-paths ["dev-src/clj"]}}
  :cljsbuild {:builds {:dev {:source-paths ["src-cljs/dreamcatcher"]
                             :jar true
                             :compiler {:output-to "js/dreamcatcher.js"
                                        ;;:optimizations :advanced
                                        :optimizations :simple
                                        :pretty-print true}}
                       :play {:source-paths ["dev-src/cljs/playground"]
                              :compiler {:output-to "js/playground.js"
                                         ;;:optimizations :advanced
                                         :optimizations :simple
                                         :pretty-print true}}
                       :server {:source-paths ["dev-src/cljs/playground-server"]
                                :compiler {:output-to "js/server.js"
                                           :target :nodejs
                                           :optimizations :simple
                                           :pretty-print true}}}})
