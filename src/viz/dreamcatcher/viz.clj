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

(defn draw-stm 
  ([stm] (draw-stm :name))
  ([stm label] (draw-stm stm label (constantly "")))
  ([stm label edge]
   (let [graph (get-viz-graph stm)] 
     (view-graph (keys graph) graph
                 :node->descriptor (fn [n] 
                                     {:label (label n)})
                 :edge->descriptor (fn [f t] {:label (edge f t)})))))


(defn print-stm 
  ([stm filename] (print-stm stm name (constantly "") filename))
  ([stm label filename] (print-stm stm label (constantly "") filename))
  ([stm label edge filename]
   (let [graph (get-viz-graph stm)]
     (save-graph 
       (keys graph) graph
       :node->descriptor (fn [n] {:label (label n)})
       :edge->descriptor (fn [f t] {:label (edge f t)})
       :filename filename))))


(comment
  (view-graph 
    (keys g) g
    :node->descriptor (fn [n] {:label n})))
