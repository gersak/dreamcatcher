(defproject kovacnica/dreamcatcher "1.0.3"
  :description "Dreamcatcher is a realy small library that
                strives to simulate state machine behavior."
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.taoensso/timbre "3.2.0"]]
  :plugins [[lein-clojars "0.9.1"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :profiles {:dev {:source-paths ["dev-src/clj"
                                  "test"]
                   :dependencies [[midje "1.6.3"]
                                  [org.clojure/tools.nrepl "0.2.3"]]}}
  :cljx {:builds [{:source-paths ["srcx"]
                   :output-path "src/"
                   :rules :clj}
                  {:source-paths ["srcx"]
                   :output-path "src-cljs/"
                   :rules :cljs}]}
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
