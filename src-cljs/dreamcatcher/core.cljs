diff --git a/project.clj b/project.clj
index 59bd6f5..a921fbd 100644
--- a/project.clj
+++ b/project.clj
@@ -1,17 +1,29 @@
 (defproject org.roribib/dreamcatcher"1.0.2-SNAPSHOT"
             :description "Dreamcatcher is a realy small library that
                strives to simulate state machine behavior."
-            :dependencies [[org.clojure/clojure "1.4.0"]]
+            :dependencies [[org.clojure/clojure "1.4.0"]
+                           [jayq "2.3.0"]
+                           [crate "0.2.4"]
+                           [hiccup "1.0.3"]]
             :plugins [[lein-cljsbuild "0.3.0"]
                       [lein-clojars "0.9.1"]]
             :source-path "src"
             :cljsbuild {:crossovers [dreamcatcher]
                         :crossover-jar true
                         :crossover-path "src-cljs";;})
-                        :builds {:dev 
-                                 {:source-paths ["src-cljs" "src"]
-                                  :jar true
-                                  :compiler {:output-to "js/dreamcatcher.js"
-                                             ;;:optimizations :advanced
-                                             :optimizations :simple
-                                             :pretty-print true}}}})
+                        :builds {:dev {:source-paths ["src-cljs/dreamcatcher" "src"]
+                                       :jar true
+                                       :compiler {:output-to "js/dreamcatcher.js"
+                                                  :optimizations :advanced
+                                                  ;;:optimizations :simple
+                                                  :pretty-print true}}
+                                 :play {:source-paths ["src-cljs/playground" "src"]
+                                        :compiler {:output-to "js/playground.js"
+                                                   ;;:optimizations :advanced
+                                                   :optimizations :simple
+                                                   :pretty-print true}}
+                                 :server {:source-paths ["src-cljs/playground-server"]
+                                          :compiler {:output-to "js/server.js"
+                                                     :target :nodejs
+                                                     :optimizations :simple
+                                                     :pretty-print true}}}})
diff --git a/src/playground.clj b/src/playground.clj
index 259ec2e..2f3647a 100644
--- a/src/playground.clj
+++ b/src/playground.clj
@@ -1,5 +1,5 @@
 (ns playground 
-  (:use dreamcatcher.core))
+  (:use [dreamcatcher.core :reload true]))
 
 (def door-stm (make-state-machine 
                 [:opened :closed (fn [_] (println "Door: Closed."))
@@ -28,23 +28,24 @@
 
 (def lemming-stm (make-state-machine [:any :waiting (fn [x]
                                                       (println (str (-> x get-data :name) ": Waiting"))
-                                                      (Thread/sleep 1000))
-                                      :any :opening (fn [x]
+                                                      (Thread/sleep (rand 1000)))
+                                      :waiting :opening (fn [x]
                                                       (do 
                                                         (println (str (-> x get-data :name) ": Trying to open the door."))
                                                         (-> x get-data :door (move :opened))))
-                                      :any :closing (fn [x]
+                                      :waiting :closing (fn [x]
                                                       (do
                                                         (println (str (-> x get-data :name) ": Closing the door"))
                                                         (-> x get-data :door (move :closed))))
-                                      :any :locking (fn [x]
+                                      :waiting :locking (fn [x]
                                                       (do
                                                         (println (str (-> x get-data :name) ": Locking"))
                                                         (-> x get-data :door get-data :lock (move :locked))))
-                                      :any :unlocking (fn [x]
+                                      :waiting :unlocking (fn [x]
                                                         (do
                                                           (println (str (-> x get-data :name)": Unlocking"))
                                                           (-> x get-data :door get-data :lock (move :unlocked))))]))
+                                      ;;:closing :die nilfn]))
 
 
 (def lock (get-machine-instance lock-stm :unlocked {:locked false :counter 0}))
@@ -65,7 +66,6 @@
 
 (def running true)
 
-(def agent-john (agent @john))
 
 (defn lemming-life [x]
   (when running
@@ -75,7 +75,19 @@
 
 (def lemmings [{:door door :name "Mirko"} {:door door :name "Stjepan"} {:door door :name "Franjo"} {:door door :name "Marko"} {:door door :name "Sinisa"}])
 
-(def lemming-agents (map #(-> (get-machine-instance lemming-stm :waiting %) deref agent) lemmings))
+(def lemming-agents (map #(-> (get-machine-instance lemming-stm :waiting %) deref give-life! agent) lemmings))
 
 (defn let-lemmings-loose []
   (doseq [x lemming-agents] (send-off x lemming-life)))
+
+;; Act testing
+(give-life! john)
+(def agent-john (agent @john))
+
+(defn act-test [x]
+  (do
+    (when running (send-off *agent* #'act-test))
+    (act! x :random)))
+
+(defn act-test-lemmings []
+  (doseq [x lemming-agents] (send-off x act-test)))
