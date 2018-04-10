(set-env!
  :source-paths #{"src-cljc"}
  :dependencies '[[adzerk/boot-cljs "1.7.228-1"]
                  ; [adzerk/bootlaces "0.1.13"]
                  [org.clojure/clojure "1.8.0"]
                  [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                  [org.clojure/clojurescript "1.7.145"]])

(require '[adzerk.boot-cljs :refer [cljs]])
; (require '[adzerk.bootlaces :refer :all])

(def +version+ "1.0.7-SNAPSHOT")

(task-options!
  pom {:project 'kovacnica/dreamcatcher
       :version +version+}
  jar {:manifest {"created-by" "Robert Gersak"}})

; (bootlaces! +version+)

(deftask build
  "Build dreamcatcher and install localy"
  []
  (set-env! 
    :resource-paths #{"src-cljc"})
  (comp (pom) (jar) (install)))

(deftask dev
  "Starts up development environment"
  []
  (set-env! :source-paths #{"dev-src" "src-cljc"})
  (repl)
  (comp 
    (wait)
    (repl :server true)))

(deftask deploy []
  (set-env! :resource-paths #{"src-cljc"})
  (comp 
    (pom)
    (jar)
    (push 
      :repo "clojars"
      :gpg-sign (not (.endsWith +version+ "-SNAPSHOT")))))
