(ns playground 
  (:use [dreamcatcher.core :reload-all true]))

(def door-stm (make-state-machine 
                [:opened :closed (fn [_] (println "Door: Closed."))
                 :closed :opened (fn [_] (println "Door: Opened."))
                 :closed :closed (fn [_] (println "Door: Already closed"))
                 :opened :opened (fn [_] (println "Door: Already opened"))]
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


(defn lemming-act [x]
  (let [choice (rand)]
    (cond
      (< choice 0.25) (-> x (move :opening) (move :waiting))
      (and (> choice 0.25) (< choice 0.5)) (-> x (move :locking) (move :waiting))
      (and (> choice 0.5) (< choice 0.75)) (-> x (move :unlocking) (move :waiting))
      (> choice 0.75) (-> x (move :closing) (move :waiting))
      :else (-> x (move :waiting)))))

(def running true)

(def agent-john (-> john give-life! agent))

(def john (-> john give-life! atom))

(defn lemming-life [x]
  (when running
    (send-off *agent* #'lemming-life))
  (lemming-act x))


(def lemmings [{:door door :name "Mirko"} {:door door :name "Stjepan"} {:door door :name "Franjo"} {:door door :name "Marko"} {:door door :name "Sinisa"}])

(def lemming-agents (map #(-> (get-machine-instance lemming-stm :waiting %) give-life! agent) lemmings))

(defn let-lemmings-loose []
  (doseq [x lemming-agents] (send-off x lemming-life)))

(defn lemming-life [x]
  (when running 
    (send-off *agent* #'lemming-life)
    (act! x :random)))

(defn let-lemmings-live []
  (doseq [x lemming-agents] (send-off x lemming-life)))
