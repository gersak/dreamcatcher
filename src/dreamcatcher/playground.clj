(ns dreamcatcher.playgrond
  (:use dreamcatcher.core))

(def door-stm (make-state-machine 
                [:opened :closed (fn [_] (println "Door: Closing doors."))
                 :closed :opened (fn [_] (println "Door: Opening doors."))
                 :closed :closed (fn [_] (println "Door: Already closed"))
                 :opened :opened (fn [_] (println "Door: Already opened"))]))

(def lock-stm (make-state-machine [:locked :unlocked (fn [x]
                                                       (do (println "Lock: Unlocked")
                                                         (assoc-data x :locked false :counter 0)))
                                   :unlocked :locked (fn [x]
                                                       (do
                                                         (println "Lock: Locked")
                                                         (assoc-data x :locked true)))
                                   :locked :locked (fn [x]
                                                     (do 
                                                       (println "Lock: Already locked...")
                                                       (swap-data x :counter inc)))
                                   :unlocked :unlocked (fn [x] (println "Lock: Already UNlocked!"))]))

(def lemming-stm (make-state-machine [:any :waiting (fn [x]
                                                      (println (str (-> x get-data :name) ": Waiting"))
                                                      (Thread/sleep 1000))
                                      :any :opening (fn [x]
                                                      (do 
                                                        (println (str (-> x get-data :name) ": Trying to open the door."))
                                                        (-> x get-data :door (move :opened))))
                                      :any :closing (fn [x]
                                                      (do
                                                        (println (str (-> x get-data :name) ": Closing the door"))
                                                        (-> x get-data :door (move :closed))))
                                      :any :locking (fn [x]
                                                      (do
                                                        (println (str (-> x get-data :name) ": Locking"))
                                                        (-> x get-data :door get-data :lock (move :locked))))
                                      :any :unlocking (fn [x]
                                                        (do
                                                          (println (str (-> x get-data :name)" :Unlocking"))
                                                          (-> x get-data :door get-data :lock (move :unlocked))))
                                      [:waiting :opening :closing] :any nilfn]))


(def lock (get-machine-instance lock-stm :unlocked {:locked false :counter 0}))

(def door (get-machine-instance door-stm :opened {:lock lock}))

(def john (get-machine-instance lemming-stm :waiting {:door door :name "John"}))
