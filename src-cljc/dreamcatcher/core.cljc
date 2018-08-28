(ns dreamcatcher.core
  #?(:cljs (:require-macros 
             [dreamcatcher.macros :refer [add-statemachine-mapping]]))
  (:require 
    #?(:clj [dreamcatcher.macros :refer [add-statemachine-mapping]])
    [dreamcatcher.util 
     :refer [get-state-mapping 
             get-transitions 
             get-validators 
             has-transition? 
             get-transition 
             get-states]]))

(defprotocol STM
  (state [this] "Returns current state of instance")
  (set-data! [this data] "Sets data. Returns instance.")
  (set-state! [this state] "Sets machine instance state. Returns instance.")
  (data [this] "Returns data state of instance")
  (stm [this] "Returns STM reference")
  (set-stm! [this stm] "Sets statemachine instance with STM")
  (context [this] "Returns STM optional context (setup/configuration)")
  (set-context! [this context] "Sets STM context"))

(defn update-data!
  "Function updates instance data applying f to current instance data & arguments"
  ([instance f]
   {:pre [(satisfies? STM instance)]}
   (set-data! instance (f (data instance))))
  ([instance f x]
   {:pre [(satisfies? STM instance)]}
   (set-data! instance (f (data instance) x)))
  ([instance f x y]
   {:pre [(satisfies? STM instance)]}
   (set-data! instance (f (data instance) x y)))
  ([instance f x y z]
   {:pre [(satisfies? STM instance)]}
   (set-data! instance (f (data instance) x y z)))
  ([instance f x y z & more]
   {:pre [(satisfies? STM instance)]}
   (set-data! instance (apply f (data instance) x y z more))))

(defn update-state!
  "Function updates instance state applying f to current instance state & arguments"
  ([instance f]
   {:pre [(satisfies? STM instance)]}
   (set-state! instance (f (state instance))))
  ([instance f x]
   {:pre [(satisfies? STM instance)]}
   (set-state! instance (f (state instance) x)))
  ([instance f x y]
   {:pre [(satisfies? STM instance)]}
   (set-state! instance (f (state instance) x y)))
  ([instance f x y z]
   {:pre [(satisfies? STM instance)]}
   (set-state! instance (f (state instance) x y z)))
  ([instance f x y z & more]
   {:pre [(satisfies? STM instance)]}
   (set-state! instance (apply f (state instance) x y z more))))

(defn update-context!
  "Function updates instance context applying f to current instance context & arguments"
  ([instance f]
   {:pre [(satisfies? STM instance)]}
   (set-context! instance (f (context instance))))
  ([instance f x]
   {:pre [(satisfies? STM instance)]}
   (set-context! instance (f (context instance) x)))
  ([instance f x y]
   {:pre [(satisfies? STM instance)]}
   (set-context! instance (f (context instance) x y)))
  ([instance f x y z]
   {:pre [(satisfies? STM instance)]}
   (set-context! instance (f (context instance) x y z)))
  ([instance f x y z & more]
   {:pre [(satisfies? STM instance)]}
   (set-context! instance (apply f (context instance) x y z more))))

(defprotocol STMMovement
  (choices? 
    [this]
    "Returns reachable states from current state
     of state machine instance in form of vector.

     First directly reachable states are calculated
     and set as first choices. If there are states in
     STM of this instance that are reachable from
     :any state other than choices than that states
     are also valid and are inserted after direct
     transitions.")
  (candidates? 
    [this]
    "Returns candidate states that are valid to exit from
     current state and transit to next-state. It is not
     a guarante that it will succeed since transition can
     change state of instance so it will not pass :any to
     :next-state validation.")
  (move 
    [this next-state]
    "Makes attempt to move machine instance to next state. Input
     argument is machine instance and 3 functions are applied.

     Order of operations is :

     1* from current state -> any state - If there is general outgoing function in current state
     2* transition fn from state -> next-state - Direct transtion between states
     3* from any state -> next-state fn - If there is general incoming function in next state

     Return value is map, that is STM instance.

     DRAGONS: Movement doesn't throw exception if move doesn't happen because
     of machine state validation. Movement either happens or id doesn't. Check state after
     movement if that is important to your setup and throw exceptions in validators
     and transitions." ))

(defprotocol STMLife
  (give-life! 
    [this] [this life-choices]
    "Give STM instance life... If no choices
     are given, than instance is free to live on
     its own. If there are choices that restrict
     machine behaviour than that rules are applied.

     Choices are supposed to be map in form:

     {:state1 [:state2 :state3 :state1 :state1]}

     This means that if machine is in :state1 first
     choice is :state2, second choice is :state3 etc.")
  (alive? [this])
  (kill! [this])
  (act! 
    [this]
    "Moves state machine to next state. If transition
     priority is defined with give-life! function, than
     those rule order is applied. Otherwise state machine
     acts free of will... "))


(defprotocol STMPath
  (to-> 
    [this target]
    "Calculates all paths from current state
     to end state")
  (reach-state 
    [this target]
    "Function strives to reach input state from
     current state of STM instance. Functions that are
     defined as transitions or indirect :any functions
     should operate on other constructs CAREFULLY! 

     Reason is: While trying to reach certain
     state with reach-state function many transitions
     are activated and end result is first possible
     path from current state to target state with all
     transitions applied inbetween.

     Somewhat -> macro with validators."))

;; State Machine is a simple map...
;; That contains transitons from one state to another.
;; It can also contain validators that evaluate if
;; transition is valid to happen or not.

;; StateMachine generation

(defn add-state [stm state]
  (assert (not (#{:state :data :stm} state)) "[:state :data :stm] are special keys")
  (swap! stm assoc state nil))

(defn remove-state [stm state]
  (swap! stm #(dissoc % state)))

(add-statemachine-mapping validator  ::validators)
(add-statemachine-mapping transition ::transitions)

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



;; DEPRECATED
(defmacro safe
  "Simple wrapping macro for easier
    definition. Wraps body in a
   function THAT returns same argument
   that was argument. Body parts are
   evaluated thorougly."
  [& body]
  `(fn  [x#]
     (do ~@body) x#))



;; DEPRECATED - use as->
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
;; (atom {::state nil :data nil :stm nil})
(defrecord STMInstance [stm state data context]
  STM
  (stm [_] stm)
  (state [_] state)
  (set-state! [this state'] (assoc this :state state'))
  (set-context! [this context'] (assoc this :context context'))
  (set-data! [this data'] (assoc this :data data'))
  (set-stm! [this stm'] (assoc this :stm stm'))
  (data [_] data)
  (context [_] context))


(defn valid-transition?
  "Computes if transition from-state to to-state is valid.
   If there is any type of validator function than function
   result decides if transition is valid. If no validator
   is provided than transition is valid.

   If validator is not a function, transition is not valid."
  [^STMInstance instance from-state to-state]
  (if-let [vf (get (::validators (get-state-mapping (stm instance) from-state)) to-state)]
    (when (fn? vf)
      (vf instance))
    true))

(defn make-machine-instance
  "Return STM instance with initial state. Return value is map
   with reference to ::stm and with parameters :data and :state
   that represents current data and current state."
  ([stm initial-state] 
   {:pre [((set (keys stm)) initial-state)]}
   (make-machine-instance stm initial-state nil))
  ([stm initial-state data]
   {:pre [((set (keys stm)) initial-state)]}
   (STMInstance. stm initial-state data nil)))

;; Instance operators
;;
;; Instance has a state that can be changed with transition
;; if transition is valid.

(defn get-reachable-states
  "Returns reachable states from positon of instance
   within state machine."
  ([^STMInstance instance]
   (get-reachable-states instance (state instance)))
  ([^STMInstance instance state]
   (->> (keys (get-transitions (stm instance) state))
        vec
        (remove (partial = :any)))))

(defn- step [^STMInstance instance from-state to-state fun]
  (when-not (nil? instance)
    (when (and
            (valid-transition? instance from-state to-state)
            (valid-transition? instance :any to-state)
            (valid-transition? instance from-state :any))
      (let [new-state (fun instance)]
        (assert (satisfies? STM instance)
                (str "Input fun takes old STM instance and produces new STM instance. Please asure that fn: " fun " returns implentation of STM protocol."))
        new-state))))

(defn state-changed? [^STMInstance state1 ^STMInstance state2]
  (let [project (juxt state data stm)]
    (boolean (some false? (map = (project state1) (project state2))))))

(defn- move-stm
  [^STMInstance instance to-state]
  (if-let [stm (stm instance)]
    (do
      (when-not (contains? stm to-state)
        (ex-info 
          (str "STM doesn't contain state " to-state)
          {:type :dreamcatcher/movement
           :instance instance
           :to-state to-state}))
      (when-not (has-transition? stm (state instance) to-state)
        (ex-info
          (str "There is no transition function from state " (state instance) " to state " to-state)
          {:type :dreamcatcher/movement
           :instance instance
           :to-state to-state}))
      (let [from-state (state instance)
            tfunction (or (get-transition stm from-state to-state) identity)
            in-fun (or
                     (get-transition stm :any to-state)
                     identity)
            out-fun (or
                      (get-transition stm from-state :any)
                      identity)]
        (if-let [new-instance (-> instance
                                  (step from-state :any out-fun)
                                  (step from-state to-state tfunction)
                                  (step :any to-state in-fun))]
          (do
            ;; Add hook for aditional utils spying on instances
            ;; Gets called only when transition already happened
            ;; and doesn't affect new-instance
            (when (state-changed? instance new-instance)
              (when-let [always (get-transition stm :any :any)]
                (when (fn? always)
                  (always instance new-instance))))
            (assoc new-instance :state to-state))
          instance)))
    (throw
      (ex-info 
        (str "There is no state machine configured for this instance: " instance)
        {:type :dreamcatcher/definition
         :instance instance}))))


(defn- get-choices
  ([^STMInstance x] (get-choices x (state x)))
  ([^STMInstance x state]
   (remove 
     #{:any}
     (if-let [fix-choices (-> x context ::life (get state))]
       fix-choices
       (-> x stm (get-transitions state) keys vec)))))

(defn- get-valid-candidates
  ([^STMInstance x] (get-valid-candidates x (state x)))
  ([^STMInstance x state]
   (when-let [choices (seq (get-choices x state))]
     (when (valid-transition? x state :any)
       (seq (filter #(and (valid-transition? x state %)
                          (valid-transition? x :any %)) choices))))))


;; State machine life and behaviour
(defn- move-to-next-choice [^STMInstance x]
  (let [choices (-> x context ::life (get (state x)))
        next-choice (first choices)]
    (if next-choice
      (let [new-state (move-stm x next-choice)]
        (update-context!
          new-state update-in [::life (state new-state)]
          #(cond
             (vector? %) (vec (rest %))
             :else (concat (rest %) (take 1 %)))))
      (throw 
        (ex-info 
          "Instance has no choice whatsoever!"
          {:instance x})))))

;; Graph tranversing
(defn from->to
  ([stm start end] (from->to stm start end true))
  ([stm start end direct?]
   ;; check if stm contains end state
   {:pre [(contains? stm end)]}
   (if (= start end)
     nil
     (let [stm (if direct? (dissoc stm :any) stm)]
       (letfn [(visited? [path state]
                 (not= -1 (.indexOf #?(:clj path :cljs (clj->js path)) state)))
               (generate-paths [current-path]
                 (let [c (last current-path)]
                   (if-not (= c end)
                     (if-let [targets (seq
                                        (concat
                                          (-> stm (get-transitions (or c start)) keys)
                                          (when-not direct? (keys (get-transitions stm :any)))))]
                       (let [expanded-paths (seq 
                                              (map 
                                                #(if-not (visited? current-path %)
                                                   (vec (conj current-path %))
                                                   current-path)
                                                targets))]
                         expanded-paths)
                       [current-path])
                     [current-path])))]
         (loop [paths [[]]]
           (let [new-paths (reduce into #{} (map generate-paths paths))]
             (if (= new-paths paths) (->> paths
                                          (remove #(= -1 (.indexOf % #?(:clj end :cljs (clj->js end))) ))
                                          (sort-by count))
               (recur new-paths)))))))))




(extend-type STMInstance
  STMMovement
  (choices? [this] (get-choices this))
  (candidates? [this] (get-valid-candidates this))
  (move [this next-state] (move-stm this next-state))
  STMLife
  (give-life!
    ([this choices] (let [proposed-life (reduce conj (array-map)
                                                (for [x (keys (dissoc (stm this) :any))]
                                                  [x (get-reachable-states this x)]))]
                      (cond-> (update-context! this assoc ::alive? true)
                        (seq proposed-life) (update-context! assoc ::life proposed-life)
                        (seq choices) (update-context! update ::life merge choices))))
    ([this] (give-life! this nil)))
  (alive? [this] (get (context this) ::alive?))
  (kill! [this] (update-context! this assoc ::alive? false))
  (act!
    ([this]
     (assert ((comp ::alive? context) this) "Instance is not alive! First give it life...")
     (loop [new-state (move-to-next-choice this)]
       (if (state-changed? this new-state)
         new-state
         (recur (move-to-next-choice new-state))))))
  STMPath
  (to->
    ([this target]
     (from->to (stm this) (state this) target true)))
  (reach-state
    ([instance state']
     (let [stm (stm instance)
           paths (to-> instance state')
           move (memoize move) ;; Do not apply transitions more than once per try
           tranverse (fn [path]
                       (loop [x instance
                              c (state instance)
                              path-left path]
                         (if (= c state') x
                           (if-not (seq path-left) nil
                             (let [next-x (move x (first path-left))]
                               (if (= (state next-x) c) nil
                                 (recur next-x (state next-x) (rest path-left))))))))]
       (some tranverse paths)))))

(defmacro multimove 
  "Move STMInstance through series of states."
  [instance & states]
  (let [movements# (map #(list `move %) states)]
    `(-> ~instance ~@movements#)))
