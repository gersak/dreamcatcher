(set-env!
  :source-paths #{"src-cljc"}
  :repositories #(conj % ["clojars-deploy" {:url "https://clojars.org/repo"
                                            :username (System/getenv "CLOJARS_USER")
                                            :password (System/getenv "CLOJARS_PASS")}])
  :dependencies '[;; BOOT DEPENDENCIES
                  [adzerk/boot-cljs "1.7.228-1"]
                  [adzerk/boot-reload "0.4.2"]
                  [adzerk/boot-cljs-repl "0.3.0"]
                  [pandeiro/boot-http "0.7.3"]
                  [com.cemerick/piggieback "0.2.1" :scope "test"]
                  [weasel "0.7.0" :scope "test"]
                  [org.clojure/tools.nrepl "0.2.12" :scope "test"]
                  [org.martinklepsch/boot-garden "1.3.0-0"]
                  ;[boot-codox "0.9.4" :scope "test"]
                  ;; PLAYGROUND DEPENDENCIES
                  [sablono "0.6.3"]
                  [org.omcljs/om "1.0.0-alpha22"]
                  [cljsjs/react "0.14.3-0"]
                  [cljsjs/react-dom "0.14.3-1"]
                  [garden "1.3.2"]
                  ;; DREAMCATCHER DEPENDENCIES
                  [org.clojure/clojure "1.8.0"]
                  [org.clojure/core.async "0.2.374"]
                  [org.clojure/clojurescript "1.8.40"]])

(require '[adzerk.boot-cljs :refer [cljs]])
(require '[pandeiro.boot-http :refer [serve]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]])
(require '[org.martinklepsch.boot-garden :refer [garden]])

(def +version+ "1.0.6-SNAPSHOT")
(def +jar+ "dreamcatcher.jar")
(def +target+ "release-files")

(task-options!
  target {:dir +target+}
  push {:repo "clojars-deploy"
        :ensure-release true
        :gpg-sign true}
  pom {:project 'kovacnica/dreamcatcher
       :version +version+
       :dependencies '[[org.clojure/clojure "1.8.0"]
                       [org.clojure/core.async "0.2.374"]
                       [org.clojure/clojurescript "1.8.40"]]}
  jar {:manifest {"created-by" "Robert Gersak"}
       :file +jar+})

;;======================================== 

;; RELEASE
(deftask build
  "Build dreamcatcher and install localy"
  []
  (comp (pom)  (jar) (install)))


(deftask push-snapshot 
  "Pushes current snapshot to clojars"
  []
  (comp 
    (build) 
    (push :repo "clojars-deploy" :ensure-release false :ensure-snapshot true :gpg-sign true)))

;;======================================== 

;; DEVELOPMENT
(deftask build-development
  "Builds development files"
  []
  (set-env! :source-paths #{"dev-src" "src-cljc"}
            :resource-paths #{"dev-resources/html"})
  (comp
    (serve
      :dir "dev-resources/html")
    (watch)
    (reload)
    (speak)
    (garden
      :output-to "css/login.css"
      :styles-var 'playground.css/login)
    (garden
      :output-to "css/bpm.css"
      :styles-var 'playground.css/dreamcatcher-bpm)
    (cljs-repl)
    (cljs)))

(deftask start-development
  "Starts up development environment"
  []
  (println "Setting up environment")
  (set-env! :source-paths #{"src-cljc" "dev-src"}
            :resource-paths #{})
  (println "Setting up Cider...")
  (cider)
  (println "Starting REPL...")
  (repl))


(deftask start-web-development
  "Starts up WEB development environment"
  []
  (task-options! 
    repl {:init-ns 'playground.core
          :skip-init true})
  (set-env! :source-paths #{"dev-src" "src-cljc"}
            :resource-paths #{})
  (repl))

