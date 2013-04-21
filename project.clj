(defproject org.roribib/dreamcatcher"1.0.2-alpha2"
            :description "Dreamcatcher is a realy small library that
               strives to simulate state machine behavior."
            :dependencies [[org.clojure/clojure "1.5.0"]]
            :plugins [[lein-cljsbuild "0.3.0"]
                      [lein-clojars "0.9.1"]]
            :source-paths ["src" "src-cljs"]
            ;;:hooks [leiningen.cljsbuild]
            :cljsbuild {:crossovers [dreamcatcher]
                        :crossover-jar true
                        :crossover-path "src-cljs";;})
                        :builds {:dev 
                                 {:source-paths ["src-cljs" "src"]
                                  :jar true
                                  :compiler {:output-to "js/dreamcatcher.js"
                                             ;;:optimizations :advanced
                                             :optimizations :simple
                                             :pretty-print true}}}})
