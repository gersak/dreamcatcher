(ns playground.core
  #?(:cljs 
     (:require-macros  
       [dreamcatcher.core :refer [defstm ]]
       [clojure.core.async :refer [go]]))
  (:require [dreamcatcher.async :refer :all]
            [dreamcatcher.core :as d 
             :refer [safe with-stm move reach-state any-state
                     make-machine-instance state-changed?
                     data update-data! state make-machine-instance defstm
                     states-other-than]]
            [clojure.core.async :as async 
             :refer [mult mix chan tap 
                     admix close! put! take! #?(:clj go)]]))

(letfn
  [(path-history [x]
     (if (map? (data x)) 
       (do
         (println "Traversed  paths: " (conj (-> x data :history) (state x)))
         (-> x 
             (update-data! update :history conj (state x))))
       x))
   (inc-counter [x]
     (update-data! x update :counter inc))
   (print-state-counter [instance]
     (let [{:keys [counter name]} (data instance)] 
       (println "Final counter state for " name " is: " counter)
       (println "Traversed path: " (-> instance data :history)))
     instance)]
  (defstm simple-stm
    ;; Transitions
    [1 2 #(as-> % x
            (do
              (println "From 1->2" (state x))
              (inc-counter x)))
     2 3 #(as-> % x
            (do
              (println "From 2->3" (state x))
              (inc-counter x)))
     1 3 #(as-> % x
            (do 
              (println "From 1->3" (state x))
              (inc-counter x)))
     3 [4 5] identity
     4 5 #(as-> % x
            (inc-counter x))
     5 6 #(as-> % x
            (do 
              (println (state x))
              (println "From 5->6")
              (inc-counter x)))
     :any 6 print-state-counter
     [1 2 3 4 5 6] :any path-history]
    ;; Validators
    [1 3 #(as-> % x
            (if (< (-> x data :counter) 1000)
              false
              true))
     3 4 (fn [_] false)])
  (defstm stm1
    [1 2 #(as-> % x
            (println "STM1 From 1->2" (state x))
            x)
     2 3 #(as-> % x
            (println "STM1 From 2->3" (state x))
            x)
     3 4 #(as-> % x
            (println "STM1 From 3->4" (state x))
            x)
     4 5 #(as-> % x
            (println "STM1 From 4->5" (state x))
            x)])
  (defstm stm2
    [1 2 #(as-> % x
            (println "STM2 From 1->2" (state x))
            x)
     2 3 #(as-> % x
            (println "STM2 From 2->3" (state x))
            x)]))

(def test-instance (wrap-async-machine simple-stm
                                       :exception-fn (fn [ex] 
                                                       (println (.getMessage ex))
                                                       (println "Data: " (ex-data ex)))))

(def sinstance (make-machine-instance 
                 simple-stm 1 
                 {:name "kifla" 
                  :counter 0}))

(comment 
  (dreamcatcher.util/get-states simple-stm)
  (map state sinstance sinstance)
  (state-changed? sinstance (move sinstance 2))
  (state-changed? sinstance (move sinstance 3))
  (inject test-instance 1 
          {:name "kifla"
           :counter 0})
  (get-state-channel test-instance 2)
  (let [c (suck test-instance 6)] 
    (async/go-loop [i (async/<! c)]
      (println "Hellooo!!!" i)
      (recur (async/<! c))))
  (data (reach-state sinstance 6))
  (state (reach-state sinstance 6))
  (-> 
    sinstance
    (move 2))
  (d/to-> sinstance 6)
  (move sinstance 2)
  (data sinstance)
  (state sinstance)
  (data (update-data! sinstance update :counter inc)))

#_(disable test-instance)

(def test-connection-instance-1 (wrap-async-machine stm1))
(def test-connection-instance-2 (wrap-async-machine stm2))

(let [end-channel (suck test-connection-instance-1 5)]
  (penetrate test-connection-instance-2 1 end-channel))



(defstm super
  ['1 '2 identity
   '2 '3 identity
   '2 '_3 identity
   '2 '__3 identity
   '3 '4 identity
   (states-other-than '1 '2 '3 '6) '6 identity
   '2 any-state identity]
  [(states-other-than '1 '2 '6) 6' (constantly false)])



(def i (make-machine-instance super '2 {}))


