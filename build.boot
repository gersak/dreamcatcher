(set-env!
  :source-paths #{"src-cljc"}
  :dependencies '[[adzerk/boot-cljs "1.7.228-1"]
                  [pandeiro/boot-http "0.7.3"]
                  [funcool/boot-codeina "0.1.0-SNAPSHOT"]
                  [adzerk/boot-reload "0.4.2"]
                  [org.clojure/clojure "1.8.0"]
                  [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                  [org.clojure/clojurescript "1.7.145"]])

(require '[adzerk.boot-cljs :refer [cljs]])
(require '[pandeiro.boot-http :refer [serve]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[funcool.boot-codeina :refer :all])

(task-options!
  pom {:project 'kovacnica/dreamcatcher
       :version "1.0.4"}
  jar {:manifest {"created-by" "Robert Gersak"}}
  apidoc {:version "1.0.4"
          :title "Dreamcatcher Core"
          :sources #{"src-cljc"}
          :description "Package for creating state machines and state machine instances"})

(deftask build
  "Build dreamcatcher and install localy"
  []
  (comp (apidoc) (pom) (jar) (install)))

(deftask start-development
  "Startsup development environment"
  []
  (println "Setting up environment")
  (set-env! :source-paths #{"dev-src" "src-cljc"})
  (println "Setting up Cider...")
  (cider)
  (println "Starting REPL...")
  (repl))
