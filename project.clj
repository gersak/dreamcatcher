(defproject dreamcatcher "1.0.1-SNAPSHOT"
            :description "Dreamcatcher is a realy small library that
               strives to simulate state machine behavior."
            :dependencies [[org.clojure/clojure "1.4.0"]]
            :plugins [[lein-cljsbuild "0.2.9"]]
            :cljsbuild {:builds [{:source-path "src/dreamcatcher"
                                  :compiler {:output-to "js/dreamcatcher.js"
                                             ;;:optimizations :advanced
                                             ;;:optimizations :whitespace
                                             :pretty-print true}}]})
