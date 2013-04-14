(ns playground.core 
  ;;(:use [dreamcatcher.core :only (give-life! make-state-machine move get-machine-instance get-data)])
  (:use [jayq.util :only (wait)])
  (:use-macros [jayq.macros :only (ready)])
  (:require [crate.core :as c]
            [dreamcatcher.core :as d]
            [dreamcatcher.util :as du]
            [jayq.core :as jq]))

(declare $body raphael act-test position2)

(def test-machine (atom nil))


(defn get-state-circle
  ([x y] (get-state-circle x y "orange"))
  ([x y color] (.. raphael 
                   (circle 0 0 30)
                   (attr (clj->js {:fill color}))
                   (transform (str "T" x "," y)))))
                   ;;(glow (clj->js {:color "black"})))))

(defn position-element [elem x y]
  (.transform elem (str "T" x "," y)))

(defn position-machine [x y] (fn [machine] (-> machine d/get-data :dot (position-element x y)))) 


(def test-stm (d/make-state-machine [:any :right (position-machine 300 200) 
                                     :any :left  (position-machine 100 200)
                                     :any :up (position-machine 200 100)
                                     :any :down (position-machine 200 300)
                                     :any :center (position-machine 100 100)
                                     :center :center d/nilfn]))

;;(def test-machine (d/get-machine-instance test-stm :up {:dot (get-state-circle 200 100 "orange")}))

;;(def act-test []
;;  (swap! test-machine d/act! :random)
;;  (wait 1000 act-test))


;; Initialization
(defn initialize []
  (.log js/console "Creating body variable")
  (def $body (jq/$ "body"))
  (jq/empty $body)
  (def raphael (js/Raphael. $body))
  ;; Define test-machine
  (reset! test-machine (d/get-machine-instance test-stm :center {:dot (get-state-circle 100 100 "orange")}))
  (swap! test-machine d/give-life!)
  (.log js/console (str "Test1: " (.indexOf (clj->js [:1 :2 :3 :24]) (clj->js :24))))
  (.log js/console (str "Test2: " (d/position [:1 :2 :3 :24] :25)))
  (.log js/console (str "Test3: " (du/get-transitions (d/get-stm @test-machine) :any)))
  (.log js/console (str "Next choice is: " (d/next-choice test-machine)))
  (swap! test-machine #(d/move % (d/next-choice @test-machine))))
  ;;(act-test))
  
  ;;(d/move test-machine :down))


(ready
  (.log js/console "Initializing")
  (initialize))
