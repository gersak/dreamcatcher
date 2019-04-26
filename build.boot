(set-env!
  ; :source-paths #{"src"}
  :dependencies '[[adzerk/boot-cljs "2.1.5"]
                  [org.clojure/clojure "1.9.0"]
                  [org.clojure/core.async "0.4.490"]
                  [org.clojure/clojurescript "1.10.516"]])

(require '[adzerk.boot-cljs :refer [cljs]])

(def +version+ "1.0.9-SNAPSHOT")

(task-options!
  pom {:project 'kovacnica/dreamcatcher
       :version +version+}
  jar {:manifest {"created-by" "Robert Gersak"}})

(deftask visual []
  (set-env! :dependencies #(concat % '[[rhizome "0.2.9"]])))

(deftask build
  "Build dreamcatcher and install localy"
  []
  (set-env! :resource-paths #{"src/core"})
  (comp (pom) (jar) (install)))

(deftask dev
  "Starts up development environment"
  []
  (set-env! :source-paths #{"dev-src" "src/core" "src/viz"}
            :dependencies #(concat % '[[rhizome "0.2.9"]]))
  (comp 
    (wait)
    (repl)))

(deftask deploy []
  (set-env! 
    :resource-paths #{"src/core"}
    :source-paths #{})
  (comp 
    (pom)
    (jar)
    (push 
      :repo "clojars"
      :gpg-sign (not (.endsWith +version+ "-SNAPSHOT")))))
