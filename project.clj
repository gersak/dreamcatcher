(defproject kovacnica/dreamcatcher "1.0.4"
  :description "Dreamcatcher is a realy small library that
               strives to simulate state machine behavior."
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/clojurescript "1.7.145"]]
  ;:aot :all
  :source-paths ["src-cljc" "test"]
  :test-paths ["test/dreamcatcher"]
  :profiles {:dev {:dependencies [[reagent "0.5.1"]
                                  [midje "1.7.0"]
                                  [ring "1.4.0"]
                                  [compojure "1.4.0"]
                                  [http-kit "2.1.19"]
                                  [prismatic/dommy "1.1.0"]
                                  [hiccup "1.0.5"]]
                   :source-paths ["dev-src/cljc"]}}
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
