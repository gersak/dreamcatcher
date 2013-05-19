(ns baloime
  (:use [dreamcatcher.core :reload-all true]
        [dreamcatcher.util :reload-all true])
  (:require [clj-time.core :as t]))

(load "baloime/jobs")

(def job-machine (make-state-machine [:start :idle identity
                                      :idle :idle (fn [x]
                                                    (println "Time: " (t/now))
                                                    (Thread/sleep (-> x get-data :repeat-every))
                                                    (swap-data! x :counter inc))
                                      :idle :end identity]
                                     [:idle :end (fn [x] (< 4 (-> x get-data :counter)))
                                      :idle :idle (fn [x] (> 5 (-> x get-data :counter)))]))

(def job-agent (agent 
                 (-> (get-machine-instance job-machine-1 :start {:counter 0 :repeat-every 2000})
                     give-life!)))

(defn job-life [x]
  (if (= :end (get-state x)) x
    (do
      (send-off *agent* #'job-life)
      (act! x))))
