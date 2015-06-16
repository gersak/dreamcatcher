(defproject kovacnica/dreamcatcher "1.0.4"
  :description "Dreamcatcher is a realy small library that
               strives to simulate state machine behavior."
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/clojurescript "0.0-3308"]]
  ;:aot :all
  ;:source-paths ["srcx" "src" "src-cljs"]
  :source-paths ["src-cljc" "test"]
  ;:plugins [[lein-figwheel "0.3.3"]]
  :test-paths ["test/dreamcatcher"]
  :test-selectors {:default (complement :integration)}
  :profiles {:dev {:dependencies [[reagent "0.5.0"]
                                  [midje "1.6.3"]
                                  [ring "1.4.0-RC1"]
                                  [compojure "1.3.4"]
                                  [http-kit "2.1.18"]
                                  [prismatic/dommy "1.1.0"]
                                  [hiccup "1.0.5"]]
                   :source-paths ["dev-src/cljs"]}}
  :cljx {:builds [{:source-paths ["srcx"]
                   :output-path "src/"
                   :rules :clj}
                  {:source-paths ["srcx"]
                   :output-path "src-cljs/"
                   :rules :cljs}]}
  :clean-targets ^{:private false} [:target-path "out" "resource/public/js"]
  :figwheel {:http-server-root  "js"
             :server-port 1337
             :ring-handler playground-server.server/app}
  :cljsbuild {:builds
              {:dev {:source-paths ["src-cljc/dreamcatcher"]
                     :jar true
                     :figwheel true
                     :compiler {:output-to "resources/public/js/dreamcatcher.js"
                                :optimizations :none
                                ;:optimizations :advanced
                                ;:optimizations :simple
                                :pretty-print false}}
               :play {:source-paths ["dev-src/cljs/playground"]
                      :figwheel true
                      :compiler {:output-to "resources/public/js/playground.js"
                                 :output-dir "resources/public/js/out"
                                 :asset-path "js/out"
                                 :main playground.core
                                 ;;:optimizations :advanced
                                 :optimizations :none
                                 ;:optimizations :simple
                                 :pretty-print true}}
               :server {:source-paths ["dev-src/cljs/playground-server"]
                        :compiler {:output-to "resources/public/js/server.js"
                                   :target :nodejs
                                   :optimizations :simple
                                   :pretty-print true}}}})
