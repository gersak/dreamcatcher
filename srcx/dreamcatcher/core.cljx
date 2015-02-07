(ns dreamcatcher.core
  (:use [dreamcatcher.util :only (get-state-mapping get-transitions get-validators has-transition? get-transition) :reload true])
  (#+clj :require #+cljs :require-macros 
         [dreamcatcher.macros :as m]
   #+clj [taoensso.timbre :as log]))



;; State Machine is a simple map...
;; That contains transitons from one state to another.
;; It can also contain validators that evaluate if 
;; transition is valid to happen or not.

;; StateMachine generation

(defn add-state [stm state] 
  (assert (not (or (= :data state) (= :state state) (= :stm state))) "[:state :data :stm] are special keys")
  (swap! stm assoc state nil))

(defn remove-state [stm state]
  (swap! stm #(dissoc % state)))

(m/add-statemachine-mapping validator :validators)
(m/add-statemachine-mapping transition :transitions)

(defn make-state-machine 
  "Input values transitions and validators are ment to
  be sequences with recuring pattern 

  [from-state to-state function from-state to-state... to-state function]

  \"function\" takes one argument and that is state
  machine instance data. Result is map."
  ([transitions] (make-state-machine transitions nil))
  ([transitions validators]
   (let [stm (atom nil)
         t (partition 3 transitions)
         v (partition 3 validators)
         states (-> (map #(take 2 %) t) flatten set)]
     (doseq [x states] (add-state stm x))
     (doseq [x t] (apply add-transition (conj x stm)))
     (doseq [x v] (apply add-validator (conj x stm)))
     @stm)))

(defmacro defstm 
  "Macro defines stm with make-stat-machine function.
  STM is persistent hash-map."
  [stm-name & [transitions validators]]
  (if-not validators
    `(def ~stm-name (make-state-machine ~transitions))
    `(def ~stm-name (make-state-machine ~transitions ~validators))))
    
(defmacro with-stm 
  "Macro defines function of one argument
  with given name.
  
  (fn [stm-name] &body)
  
  Make sure that return result is transformed
  state machine instance"
  [stm-name & body]
  `(fn [~stm-name]
     (do ~@body)))

;; Machine instance is map that represents
;; "real-machine" that has real state and real data
;; and has transitions defined in stm input argument
;;
;; (atom {:state nil :data nil :stm nil})

(declare get-data)

(defn get-stm [instance]
  (:stm instance))

(defn valid-transition? 
  "Computes if transition from-state to to-state is valid.
  If there is any type of validator function than function
  result decides if transition is valid. If no validator
  is provided than transition is valid.

  If validator is not a function, transition is not valid."
  [instance from-state to-state]
  (if-let [vf (get (:validators (get-state-mapping (get-stm instance) from-state)) to-state)]
    (when (fn? vf) 
#+clj (log/debug from-state " -> " to-state " Valid? " (vf instance))
      (vf instance))
    (do
#+clj (log/debug from-state " -> " to-state " Valid? true")
     true)))

(defn invalid [_] false)

(defn get-machine-instance 
  "Return STM instance with initial state. Return value is map
  with reference to :stm and with parameters :data and :state
  that represents current data and current state."
  ([stm initial-state] (get-machine-instance stm initial-state nil))
  ([stm initial-state data]
   (hash-map :stm stm :state initial-state :data data)))


;; Instance operators
;;
;; Instance has a state that can be changed with transition
;; if transition is valid. 

(defn get-state [instance]
  (:state instance))

(defn get-reachable-states 
  "Returns reachable states from positon of instance
  within state machine."
  [instance]
  (keys (get-transitions (get-stm instance) (get-state instance))))

(defn reset-state! [instance new-state]
  "Resets STM state. STM data is not changed"
  (assoc instance :state new-state))

(defn get-data [instance]
  "Returns STM instance data"
  (:data instance))

(defn assoc-data [instance & new-data]
  "Associates STM instance with data"
  (assoc instance :data (apply assoc (get-data instance) (seq new-data))))

(defn swap-data! 
  "Swaps data in STM with function applied
  to current data in STM. First argument should
  be function."
  [instance keywrd & args]
  (if-let [f (if (fn? (first args)) (first args))]
    (if (> 2 (count args))
      (assoc-data instance keywrd (-> instance get-data (get keywrd) f))
      (assoc-data instance keywrd (apply f (-> instance get-data (get keywrd)) (rest args))))))

(defn remove-data [instance & keywords]
  "Removes data from STM"
  (update-in instance [:data] (fn [_] (apply dissoc (get-data instance) keywords))))

(defn clear-data [instance]
  "Removes all data from STM"
  (assoc instance :data nil))

;; Higher order functions
(defn- step [instance from-state to-state fun]
  (when-not (nil? instance)
    (when (valid-transition? instance from-state to-state)
      (fun instance))))

(defn move 
  "Makes attempt to move machine instance to next state. Input
  argument is machine instance and 3 functions are applied.

  Order of operations is :

  1* from current state -> any state - If there is general outgoing function in current state
  2* transition fn from state -> next-state - Direct transtion between states
  3* from any state -> next-state fn - If there is general incoming function in next state

  Return value is map, that is STM instance."
  [instance to-state]
  (if-let [stm (get-stm instance)]
    (do
      (assert (contains? stm to-state) (str "STM doesn't contain " to-state " state"))
      (let [from-state (get-state instance)
            tfunction (or (get-transition stm from-state to-state) identity)
            in-fun (or 
                     (get-transition stm :any to-state)
                     (get-transition stm "any" to-state)
                     identity)
            out-fun (or 
                      (get-transition stm from-state :any)
                      (get-transition stm from-state "any")
                      identity)]
        (assert (or tfunction in-fun) (str "There is no transition function from state " from-state " to state " to-state))
        (if-let [new-instance (-> instance
                                  (step from-state :any out-fun)
                                  (step from-state to-state tfunction)
                                  (step :any to-state in-fun))]
          (assoc new-instance :state to-state)
          instance)))
    (assert false (str "There is no state machine configured for this instance: " instance))))


(defn get-choices 
  "Returns reachable states from current state 
  of state machine instance in form of vector.

  First directly reachable states are calculated
  and set as first choices. If there are states in
  STM of this instance that are reachable from
  :any state other than choices than that states
  are also valid and are inserted after direct 
  transitions."
  ([x] (get-choices x (get-state x)))
  ([x state]
   (if-let [fix-choices (get (x :life) state)]
     (let [current->any (-> x get-stm (get-transitions :any) keys)
           current->direct (-> x get-stm (get-transitions state) keys)
           all-states (set (concat current->direct current->any))]
       (vec (filter all-states fix-choices)))
     ;(-> (filter #(contains? 
     ;               (set (into (->  keys vec)
     ;                          (-> (get-transitions (get-stm x) state) keys vec))) %) fix-choices) vec)
     (let [direct-transitions (-> (get-transitions (get-stm x) state) keys vec)
           available? (fn [x y] (not= -1 (.indexOf 
                                        #+clj x #+cljs (clj->js x) 
                                        #+clj y #+cljs (clj->js y))))
           any-transitions (-> (get-transitions (get-stm x) :any) keys)
           ; Experimenting
           ;any-transitions (remove #(= state %) any-transitions)
           sum-transitions (reduce (fn [x y] (if-not (available? x y) (conj x y) x)) direct-transitions any-transitions)]
       sum-transitions))))

(defn get-valid-candidates
  "Returns candidate states that are valid to exit from
  current state and transit to next-state. It is not
  a guarante that it will succeed since transition can
  change state of instance so it will not pass :any to
  :next-state validation."
  ([x] (get-valid-candidates x (get-state x)))
  ([x state]
   (when-let [choices (seq (get-choices x state))]
     (when (valid-transition? x state :any)
     (seq (filter (partial valid-transition? x state) choices))))))

;; State machine life and behaviour
(defn give-life!
  "Give STM instance life... If no choices
  are given, than instance is free to live on
  its own. If there are choices that restrict
  machine behaviour than that rules are applied.

  Choices are supposed to be map in form:

  {:state1 [:state2 :state3 :state1 :state1]}

  This means that if machine is in :state1 first
  choice is :state2, second choice is :state3 etc." 
  ([x] (give-life! x nil))
  ([x choices] (assoc x :alive? true :life choices :last-choice nil :last-state nil)))


(defn kill! [x]
  (assoc x :alive false))


(defn ^:private move-to-next-choice [x next-choice]
  (let [state (get-state x)
        x (move x next-choice)]
    (if (and (= (get-state x) next-choice) (not= state next-choice))
      (assoc x :last-state state :last-choice nil)
      (assoc x :last-choice next-choice))))


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
   (if-let [instance x]
     (do
       (assert (:alive? instance) "Instance is not alive! First give it life...")
       (let [available-choices (get-choices instance)]
         (if (seq available-choices)
           (let [choice (case m-character
                          :clockwise (available-choices 
                                       (-> (.indexOf 
                                             #+clj available-choices 
                                             #+cljs (clj->js available-choices)
                                             (or (:last-choice instance) (:last-state instance)))
                                           inc
                                           (rem (count available-choices))))
                          :random (available-choices (-> available-choices count rand int))
                          :fixed (available-choices (or (:last-choice instance) 0)))] 
       #+clj (log/debug "Acting in state : " (get-state x) " . Next choice: " choice)
             (move-to-next-choice x choice)))))
     (assert false "This is not STM instance"))))


;; Graph tranversing
(defn get-paths-from 
  "Calculates all reachable states from start
  state of STM"
  [stm start]
  (let [stm (dissoc stm :any)
        direct-nodes (fn [x] (map #(-> stm (get-transitions %) keys) x))
        visited? (fn [path state] (not= -1 (.indexOf #+clj path  #+cljs (clj->js path) state)))
        generate-paths (fn [current-path]
                         (let [c (last current-path)]
                           (if-let [targets (seq (-> stm (get-transitions c) keys))]
                             (map #(when-not (visited? current-path %)
                                     (vec (conj current-path %))) targets)
                             [current-path])))]
    (loop [paths [[start]]]
      (let [new-paths (reduce into [] (map generate-paths paths))]
        (if (= new-paths paths) (->> paths 
                                     (remove nil?) 
                                     (remove #(= [start] %))
                                     (sort-by count)) 
          (recur new-paths))))))

(defn from->to  
  "Calculates all paths from start state
  to end state"
  [stm start end]
  (filter #(not= -1 (.indexOf #+clj % #+cljs (clj->js %) end)) (get-paths-from stm start)))

(defn reach-state 
  "Function strives to reach input state from
  current state of STM instance. Functions that are
  defined as transitions or indirect :any functions
  SHOULD NOT operate on other constructs.

  Reason is: While trying to reach certain
  state with reach-state function many transitions
  are activated and end result is first possible
  path from current state to target state with all
  transitions applied inbetween.

  Somewhat -> macro with validators."
  ([instance state & {:keys [recuring?] :or {recuring? false}}]
   (if-not recuring?
     (let [stm (get-stm instance)
           paths (from->to stm (get-state instance) state)
           move (memoize move) ;; Do not apply transitions more than once per try
           tranverse (fn [path]
                       (loop [x instance
                              c (get-state instance)
                              path-left (rest path)]
                         (if (= c state) x
                           (if-not (seq path-left) nil
                             (let [next-x (move x (first path-left))]
                               (if (= (get-state next-x) c) nil
                                 (recur next-x (get-state next-x) (rest path-left))))))))]
       (some tranverse paths))
     (loop [i instance]
       (if (= state (get-state i))
         i
         (recur (act! i)))))))
