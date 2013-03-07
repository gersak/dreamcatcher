(ns dreamcatcher.util)

(defn get-state-mapping [stm state]
  (get stm state))

(defn get-transitions [stm state]
  (do 
    (:transitions (get-state-mapping stm state))))

(defn get-validators [stm state]
  (do 
    (:validators (get-state-mapping stm state))))
