(ns dreamcatcher.core
  (:use [dreamcatcher.util :only (get-state-mapping get-transitions get-validators has-transition? get-transition) :reload true])
  (:require;*CLJSBUILD-REMOVE*;-macros
            [dreamcatcher.macros :as m]))



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


(m/add-statemachine-mapping validator :validators)
(m/add-statemachine-mapping transition :transitions)

(defn make-state-machine 
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
         v (partition 3 validators)
         states (-> (map #(take 2 %) t) flatten set)]
     (doseq [x states] (add-state stm x))
     (doseq [x t] (apply add-transition (conj x stm)))
     (doseq [x v] (apply add-validator (conj x stm)))
     (if dynamic stm @stm))))



;; Machine instance is different atom that represents
;; "real-machine" that has real state and real data
;;
;; (atom {:state nil :data nil :stm nil})
;;
;; I can't  see any other easy way to have many instances
;; conveying rules of one state machine autamata.

(declare get-data)


(defn nilfn [x] x)

(defn get-stm [instance]
  (if (map? instance) @(:stm instance) @(:stm @instance)))

(defn valid-transition? 
  "Computes if transition from-state to to-state is valid.
  If there is any type of validator function will try to
  evaluate if transition is valid based on current data.
  
  If there is no validator than transition is valid.
  
  If validator is not a function, transition is valid."
  [instance from-state to-state]
  (if-let [vf (get (:validators (get-state-mapping (get-stm instance) from-state)) to-state)]
    (when (fn? vf) (vf (get-data instance)))
    true))

(defn get-machine-instance 
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

(defn get-state [instance]
  (if (map? instance) (:state instance) (:state @instance)))

(defn get-reachable-states 
  "Returns reachable states from positon of instance
  wihtin state machine."
  [instance]
  (keys (get-transitions (get-stm instance) (get-state instance))))

(defn reset-state! [instance new-state]
  (if (map? instance) (assoc instance :state new-state) (swap! instance assoc :state new-state))) 

(defn get-data [instance]
  (if (map? instance) (:data instance) (:data @instance)))

(defn assoc-data [instance & new-data]
  (if (map? instance)
    (apply assoc (get-data instance) (list new-data))
    (swap! instance assoc :data (apply assoc (get-data instance) new-data))))

(defn swap-data [instance keywrd function]
  (when (fn? function)
    (assoc-data instance keywrd (-> instance get-data (get keywrd) function))))

(defn remove-data [instance & keywords]
  (let [data (get-data instance)]
    (if (map? instance)
      (update-in instance [:data] (fn [_] (apply dissoc data keywords)))
      (swap! instance #(update-in % [:data] (fn [_] (apply dissoc data keywords)))))))


;; Higher order functions
(defn move 
  "Makes attemp to move machine instance to next state. Instance
  input has to be clojure.lang.Atom

  First transition function is called and input value for 
  transition function is instance itself. So during transition it is possible
  to change data in this instance. If there is no direct transition function
  from-state to-state than valid transition is from state :any to \"to-state\".

  When STM instance is moving \"from-state\" to \"to-state\" it will 
  make call for transition from state :any to \"to-state\" with value of
  @instance before transition. 

  Last step of moving \"from-state\" to  \"to-state\" is calling transition
  from \"from-state\" to :any state with value of @instance before transition.

  Return value is changed instance"
  [instance to-state]
  (if-let [stm (get-stm instance)]
    (do
      (assert (contains? stm to-state) (str "STM doesn't contain " to-state " state"))
      (if (valid-transition? instance (get-state instance) to-state)
        (let [from-state (get-state instance) 
              tfunction (get-transition stm from-state to-state)
              in-fun (or 
                       (get-transition stm :any to-state)
                       (get-transition stm "any" to-state))
              out-fun (or 
                        (get-transition stm from-state :any)
                        (get-transition stm from-state "any"))
              machine-photo (if (map? instance) instance @instance)]
          (assert (or tfunction in-fun) (str "There is no transition function from state " from-state " to state " to-state))
          (when out-fun (out-fun machine-photo)) 
          (when tfunction (tfunction instance))
          (when in-fun (in-fun machine-photo))
          (if (map? instance) 
            (assoc instance :state to-state)
            (do
              (swap! instance assoc :state to-state)
              instance)))))
    (assert false "There is no state machine configured for this instance")))



(defn get-choices 
  "Returns reachable states from current state 
  of state machine instance in form of vector.

  First directly reachable states are calculated
  and set as first choices. If there are states in
  STM of this instance that are reachable from
  :any state than that choices are also valid
  and are inserted after direct transitions."
  ([x] (get-choices x (get-state x)))
  ([x state]
   (if-let [fix-choices (-> (if (map? x) x @x) :life state)]
     ;; If choices are fixed than return all valid choices
     (-> (filter #(contains? 
                    (set (into (-> (get-transitions (get-stm x) :any) keys vec)
                               (-> (get-transitions (get-stm x) state) keys vec)))
                    %) fix-choices) vec)
     (let [direct-transitions (-> (get-transitions (get-stm x) state) keys vec)
           available? (fn [x y] (if (= -1 (.indexOf x y)) false true))
           any-transitions (-> (get-transitions (get-stm x) :any) keys vec)
           sum-transitions (reduce (fn [x y] (if-not (available? x y) (conj x y) x)) direct-transitions any-transitions)]
       sum-transitions))))


;; State machine life and behaviour
(defn give-life!
  "Give STM instance life... If no choices
  are given than instance is free to live on
  its own. If there are choices that restrict
  machine behaviour than that rules are applied.

  Choices are supposed to be map in form:

  {:state1 [:state2 :state3 :state1]}

  This means that if machine is in :state1 first
  choice is :state2, second choice is :state3 etc." 
  ([x] (give-life! x nil))
  ([x choices] (if (map? x) 
                 (assoc x :alive? true :life choices :last-choice nil :last-state nil)
                 (do 
                   (swap! x assoc :alive? true :life choices :last-choice nil :last-state nil)
                   x))))

(defn kill! [x]
  (if (map? x) 
    (assoc x :alive false)
    (do
      (swap! x assoc :alive false)
      x)))

(defn ^:private move-to-next-choice [x next-choice]
  (let [state (get-state x)
        last-choice (:last-choice x)
        x (move x next-choice)]
    (if (= (get-state x) next-choice)
      (if (map? x)
        (assoc x :last-state state :last-choice nil)
        (do (swap! x assoc :last-state state :last-choice nil) x))
      (if (map? x)
        (assoc x :last-choice next-choice)
        (do (swap! x assoc :last-choice next-choice) x)))))


(defn act! 
  "Moves state machine to next state. If transition
  priority is defined with give-life! function than
  that rules are applied. Otherwise state machine 
  acts free of will...

  Free will can be 
  :clockwise
  :fixed
  :random"
  ([x] (act! x :clockwise))
  ([x m-character]
   (if-let [instance (if (map? x) x @x)]
     (do
       (assert (:alive? instance) "Instance is not alive! First give it life...")
       (let [available-choices (get-choices instance)]
         (case m-character
           :clockwise (move-to-next-choice x (available-choices (-> (.indexOf available-choices (or (:last-choice instance) (:last-state instance) -1))
                                                                    inc
                                                                    (rem (count available-choices)))))
           :random (move-to-next-choice x (available-choices (-> available-choices count rand int)))
           :fixed (move-to-next-choice x (available-choices (or (:last-choice instance) 0))))))
     (assert false "This is not STM instance"))))
