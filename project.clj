(defproject org.roribib/dreamcatcher"1.0.2-SNAPSHOT"
            :description "Dreamcatcher is a realy small library that
               strives to simulate state machine behavior."
<<<<<<< HEAD
            :dependencies [[org.clojure/clojure "1.4.0"]
                           [com.cemerick/clojurescript.test "0.0.2"]]
            :plugins [[lein-cljsbuild "0.3.0"]]
=======
            :dependencies [[org.clojure/clojure "1.4.0"]]
            :plugins [[lein-cljsbuild "0.3.0"]
                      [lein-clojars "0.9.1"]]
>>>>>>> 243a3351dee406178aa83bdad60ec8f72af61d5a
            :source-path "src"
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
