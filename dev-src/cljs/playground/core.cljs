(ns playground.core
  (:use-macros 
    [dommy.macros :only [sel sel1]])
  (:use [dommy.template :only [node]])
  (:require [dommy.core :as d]
            [dreamcatcher.core :as dc]))

(declare paper dc-tester dc-tester2)

(defn draw-circle [circle x y]
  (when circle (.remove circle))
  (.. (.circle paper x y 20) (attr "fill" (str "rgb(" (rand 100) "%," (rand 100) "%," (rand 100) "%)"))))

(defn draw-circle-rel [machine +x +y]
  (let [circle (->  machine dc/get-data :circle)
        [x y] [(-> machine dc/get-data :x) (-> machine dc/get-data :y)]]
    (when circle (.remove circle))
    (dc/swap-data! machine :circle draw-circle (+ x +x) (+ y +y))))

(def dc-stm (dc/make-state-machine [:waiting :left (fn [x] (draw-circle-rel x -100 0))
                                    :waiting :right (fn [x] (draw-circle-rel x 100 0))
                                    :waiting :up (fn [x] (draw-circle-rel x 0 -100))
                                    :waiting :down (fn [x] (draw-circle-rel x 0 100))
                                    :any :waiting (fn [x] (draw-circle-rel x 0 0))]))


;; Statless acting
(defn dc-tester [x]
  (js/setTimeout (fn [] (dc-tester (dc/act! x))) (+ 1000 (rand 5000))))

(defn init []
  (let [body (sel1 :body)
        new-node (node [:h1 "Hello from Dommy"])
        cross-machines (for [x [200 400 600 800 1000 1200] y [200 300 400 500 600]] 
                         (dc/get-machine-instance dc-stm :waiting {:circle (draw-circle nil x y)
                                                                   :x x
                                                                   :y y}))]
    (def paper (js/Raphael "STM" 1500 1000))
    (doseq [x cross-machines] (-> x dc/give-life! dc-tester))))
