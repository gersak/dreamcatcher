(ns dreamcatcher.util)

(defn get-states
  "Function returns valid states of STM.
  Returned states do not include :any state
  since it is not deterministic."
  [stm]
  (-> stm keys set (disj :any)))

(defn get-state-mapping [stm state]
  (when stm
    (get stm state)))

(defn get-transitions [stm state]
  (when stm
    (get (get-state-mapping stm state) :dreamcatcher.core/transitions)))

(defn has-transition? [stm from-state to-state]
  (fn? (-> stm
           (get from-state)
           :dreamcatcher.core/transitions
           (get to-state))))

(defn get-transition [stm from-state to-state]
  (when (has-transition? stm from-state to-state)
    (-> stm (get from-state) :dreamcatcher.core/transitions (get to-state))))

(defn get-validators [stm state]
  (:dreamcatcher.core/validators (get-state-mapping stm state)))

