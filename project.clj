(defproject org.roribib/dreamcatcher"1.0.2-SNAPSHOT"
  :description "Dreamcatcher is a realy small library that
               strives to simulate state machine behavior."
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [jayq "2.3.0"]
                 [crate "0.2.4"]
                 [hiccup "1.0.3"]]
  :plugins [[lein-cljsbuild "0.3.0"]
            [lein-clojars "0.9.1"]]
  :source-path "src"
  :cljsbuild {:builds {:dev {:source-paths ["src-cljs/dreamcatcher" "src"]
                             :jar true
                             :compiler {:output-to "js/dreamcatcher.js"
                                        ;;:optimizations :advanced
                                        :optimizations :simple
                                        :pretty-print true}}
                       :play {:source-paths ["src-cljs/playground" "src"]
                              :compiler {:output-to "js/playground.js"
                                         ;;:optimizations :advanced
                                         :optimizations :simple
                                         :pretty-print true}}
                       :server {:source-paths ["src-cljs/playground-server"]
                                :compiler {:output-to "js/server.js"
                                           :target :nodejs
                                           :optimizations :simple
                                           :pretty-print true}}}})
