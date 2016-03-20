(ns playground.core
  #?(:cljs (:require-macros  [clojure.core.async :refer [go]]))
  #?(:clj
      (:require [dreamcatcher.async :refer :all]
                [dreamcatcher.core :refer [safe defstm with-stm move state-changed? make-machine-instance data? state? make-machine-instance]]
                [clojure.core.async :as async :refer [mult mix chan tap admix close! put! take! go]])
     :cljs
     (:require [dreamcatcher.async :refer :all]
               [dreamcatcher.core :refer [move state-changed? make-machine-instance data? state? make-machine-instance]]
               [cljs.core.async :as async :refer [mult mix chan tap admix close! put! take!]])))

(letfn
    [(path-history [x]
       (if (-> x :data map?)
         (do
           (println "Traversed  paths: " (conj (-> x data? :history) (state? x)))
           (-> x (update-in [:data :history] conj (state? x))))
         x))
     (inc-counter [x]
       (update-in x [:data :counter] inc))
     (print-state-counter [{{:keys [counter name]} :data :as stm-state}]
       (println "Final counter state for " name " is: " counter)
       stm-state)]
    (defstm simple-stm
      ;; Transitions
      [1 2 (with-stm x
             (println "From 1->2" (state? x))
             (inc-counter x))
       2 3 (with-stm x
             (println "From 2->3" (state? x))
             (inc-counter x))
       1 3 (with-stm x
             (println "From 1->3" (state? x))
             (inc-counter x))
       3 [4 5] identity
       4 5 (with-stm x
             (inc-counter x))
       5 6 (with-stm x
             (println (state? x))
             (println "From 5->6")
             (inc-counter x))
       :any 6 print-state-counter
       [1 2 3 4 5 6] :any path-history #_(safe (println "HERE"))]
      ;; Validators
      [1 3 (fn [_] false)]))

(def test-instance (wrap-async-machine simple-stm))

(inject test-instance 1 {:counter 0})
