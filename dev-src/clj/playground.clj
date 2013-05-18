(ns playground 
  (:use [dreamcatcher.core :reload-all true]
        hiccup.core
        hiccup.page
        [hiccup.element :only (javascript-tag)]))

(def door-stm (make-state-machine 
                [:opened :closed (fn [x] (println "Door: Closed.") x)
                 :closed :opened (fn [x] (println "Door: Opened.") x)
                 :closed :closed (fn [x] (println "Door: Already closed") x)
                 :opened :opened (fn [x] (println "Door: Already opened") x)]
                [:opened :closed (fn [x] (if (-> x :lock deref get-data :locked) 
                                           (do (println "You cannot close locked doors. First unlock it!") false)
                                           true))
                 :closed :opened (fn [x] (if (-> x :lock deref get-data :locked)
                                           (do (println "You cannot open locked doors. First unlock it!") false)
                                           true))]))

(def lock-stm (make-state-machine [:locked :unlocked (fn [x]
                                                       (do (println "Lock: Click,click... Unlocked")
                                                           (assoc-data x :locked false :counter 0)))
                                   :unlocked :locked (fn [x]
                                                       (do
                                                         (println "Lock: Click,click... Locked")
                                                         (assoc-data x :locked true)))
                                   :locked :locked (fn [x]
                                                     (do 
                                                       (println "Lock: Already locked...")
                                                       (swap-data! x :counter inc)))
                                   :unlocked :unlocked (fn [x] (println "Lock: Already UNlocked!") x)]))

(def lemming-stm (make-state-machine [:any :waiting (fn [x]
                                                      (println (str (-> x get-data :name) ": Waiting"))
                                                      (Thread/sleep (+ 1000 (rand 1000)))
                                                      x)
                                      :waiting :opening (fn [x]
                                                          (do 
                                                            (println (str (-> x get-data :name) ": Trying to open the door."))
                                                            (-> x get-data :door (move :opened))
                                                            x))
                                      :waiting :closing (fn [x]
                                                          (do
                                                            (println (str (-> x get-data :name) ": Closing the door"))
                                                            (-> x get-data :door (move :closed))
                                                            x))
                                      :waiting :locking (fn [x]
                                                          (dosync
                                                            (println (str (-> x get-data :name) ": Locking"))
                                                            (alter (-> x get-data :door get-data :lock) move :locked))
                                                          x)
                                      :waiting :unlocking (fn [x]
                                                            (dosync
                                                              (println (str (-> x get-data :name)": Unlocking"))
                                                              (alter (-> x get-data :door get-data :lock) move :unlocked)
                                                              x))]))


(def lock (ref (get-machine-instance lock-stm :unlocked {:locked false :counter 0})))

(def door (get-machine-instance door-stm :opened {:lock lock}))

(def john (get-machine-instance lemming-stm :waiting {:door door :name "John"}))


(def running true)

(def agent-john (-> john give-life! agent))

(def john (-> john give-life! atom))


(def lemmings [{:door door :name "Mirko"} {:door door :name "Stjepan"} {:door door :name "Franjo"} {:door door :name "Marko"} {:door door :name "Sinisa"}])

(def lemming-agents (map #(-> (get-machine-instance lemming-stm :waiting %) give-life! agent) lemmings))

(defn lemming-life [x]
  (when running 
    (send-off *agent* #'lemming-life)
    (act! x :random)))

(defn let-lemmings-loose []
  (doseq [x lemming-agents] (send-off x lemming-life)))

(defn let-lemmings-live []
  (doseq [x lemming-agents] (send-off x lemming-life)))



