(ns dreamcatcher.util)

(defn get-states [stm]
  (-> stm keys))

(defn get-state-mapping [stm state]
  (when stm
    (get stm state)))

(defn get-transitions [stm state]
    (:transitions (get-state-mapping stm state)))

(defn has-transition? [stm from-state to-state]
  (fn? (-> stm
         (get from-state)
         :transitions
         (get to-state))))

(defn get-transition [stm from-state to-state]
  (when (has-transition? stm from-state to-state)
    (-> stm (get from-state) :transitions (get to-state))))

(defn get-validators [stm state]
    (:validators (get-state-mapping stm state)))
