(ns dreamcatcher.core
  (:use [dreamcatcher.util :only (get-state-mapping get-transitions get-validators)])
  (:use-macros [dreamcatcher.macros :only (add-statemachine-mapping) :reload true]))




;; State Machine is a simple atom...
;; That contains transitons from one state to another.
;; It can also contain validators that evaluate if 
;; transition is valid to happen or not.
;; And that is it... Nothing more

;; StateMachine generation
(defn add-state [stm state] 
  (do 
    (assert (not (or (= :data state) (= :state state) (= :stm state))) "[:state :data :stm] are special keys")
    (swap! stm #(merge % (hash-map state nil)))))

(defn remove-state [stm state]
  (swap! stm #(dissoc % state)))


(add-statemachine-mapping validator :validators)
(add-statemachine-mapping transition :transitions)

(defn ^:export make-state-machine 
  "Input values transitions and validators are ment to
  be sequences with recuring pattern 

  [from-state to-state function from-state to-state... to-state function]

  \"function\" takes one argument and that is state
  machine instance data"
  ([transitions] (make-state-machine transitions nil))
  ([transitions validators] (make-state-machine transitions validators true))
  ([transitions validators dynamic]
   (let [stm (atom nil)
         t (partition 3 transitions)
         states (->> (map #(take 2 %) t) (map flatten) flatten set)]
     (println t)
     (doseq [x states] (add-state stm x))
     (doseq [x t] (apply add-transition (conj x stm)))
     (if dynamic stm @stm))))



;; Machine instance is different atom that represents
;; "real-machine" that has real state and real data
;;
;; (atom {:state nil :data nil :stm nil})
;;
;; I can't  see any other easy way to have many instances
;; conveying rules of one state machine autamata.



(defn nilfn [_] nil)

(defn get-stm [instance]
  (let [stm (:stm @instance)]
    (when stm
      (if (map? stm) stm @stm))))

(defn ^:export get-reachable-states [instance]
  "Returns reachable states from positon of instance
  wihtin state machine."
  (keys (get-transitions instance (get-stm instance))))


(defn ^:export valid-transition? [instance from-state to-state]
  "Computes if transition from-state to to-state is valid.
  If there is any type of validator function will try to
  evaluate if transition is valid based on current data.
  
  If there is no validator than transition is valid.
  
  If validator is not a function, transition is valid."
  (if-let [vf (get (:validators (get-state-mapping (get-stm instance) from-state)) to-state)]
    (when (fn? vf) (vf (:data instance)))
    true))

(defn ^:export get-machine-instance 
  "Return STM instance with initial state. Return value is an atom
  with reference to :stm and with parameters :data and :state
  that represents current data and current state."
  ([stm initial-state] (get-machine-instance stm initial-state nil))
  ([stm initial-state data]
   (let [instance (atom (hash-map :stm stm :state initial-state :data data))]
     instance)))


;; Instance operators
;;
;; Instance has a state that can be changed with transition
;; if transition is valid. Validation is configured in 

(defn ^:export get-state [instance]
  (:state @instance))

(defn ^:export get-data [instance]
  (:data @instance))

(defn ^:export assoc-data [instance new-data]
  (swap! instance assoc :data (merge (get-data instance) new-data)))

(defn ^:export reset-state! [instance new-state]
  (swap! instance assoc :state new-state))

(defn ^:export move [instance to-state]
  "Makes attemp to move machine instance to next state."
  (if-let [stm (get-stm instance)] 
    (do
      (assert (contains? stm to-state) (str "STM doesn't contain " to-state "state"))
      (when (valid-transition? instance (get-state instance) to-state)
        (let [from-state (get-state instance) 
              tfunction (get (get-transitions stm from-state) to-state)]
          (assert tfunction (str "There is no transition function from state " from-state " to state " to-state))
          (tfunction instance)
          (swap! instance assoc :state to-state)))
      instance)
    nil))



(def test-stm (make-state-machine [:opened :closed (fn [_] (println "Closing!"))
                                   :closed :opened (fn [_] (println "Opening!"))
                                   [:opened :closed] :stuck (fn [_] (println "Stuck!"))
                                   :stuck [:opene :closed] (fn [_] (println "Breakthrough!"))]))
