(ns dreamcatcher.util)

(defn get-state-mapping [stm state]
  (when stm
    (get stm state)))

(defn get-transitions [stm state]
    (:transitions (get-state-mapping stm state)))

(defn get-validators [stm state]
    (:validators (get-state-mapping stm state)))
