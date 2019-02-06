(ns dreamcatcher.viz
  (:require 
    [rhizome.viz :refer :all]
    [dreamcatcher.core :refer [get-states get-transitions]]))


(defn get-viz-graph [stm]
  (let [states (get-states stm)]
    (reduce
      (fn [r state]
        (assoc r state (keys (get-transitions stm state))))
      {}
      states)))

(defn draw-stm [stm]
  (let [graph (get-viz-graph stm)] 
    (view-graph (keys graph) graph
                :node->descriptor (fn [n] {:label (name n)}))))


(defn print-stm [stm filename]
  (let [graph (get-viz-graph stm)]
    (save-graph 
      (keys graph) graph
      :node->descriptor (fn [n] {:label (name n)})
      :filename filename)))


(comment
  (view-graph 
    (keys g) g
    :node->descriptor (fn [n] {:label n})))
