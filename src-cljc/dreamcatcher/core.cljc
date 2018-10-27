(ns dreamcatcher.core
  #?(:cljs (:require-macros 
             [dreamcatcher.macros :refer [add-statemachine-mapping]]))
  (:refer-clojure :exclude [get-validator])
  (:require 
    #?(:clj [dreamcatcher.macros :refer [add-statemachine-mapping]])))


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
    any state other than choices than that states
    are also valid and are inserted after direct
    transitions.")
  (candidates? 
    [this]
    "Returns candidate states that are valid to exit from
    current state and transit to next-state. It is not
    a guarante that it will succeed since transition can
    change state of instance so it will not pass any to
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
    of machine state validation. Movement either happens or it doesn't. Check state after
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
    defined as transitions or indirect any-state functions
    should operate on other constructs CAREFULLY! 

    Reason is: While trying to reach certain
    state with reach-state function many transitions
    are activated and end result is first possible
    path from current state to target state with all
    transitions applied inbetween.

    Somewhat -> macro with validators."))


(def any-state ::any)


(def any-state? (partial = any-state))


(def 
  ^{:dynamic true
    :doc "Used by 'make-state-machine' function to create STM."} 
  *stm-constructor* nil)

(defn has-state? [stm state]
  {:pre [(map? stm)]}
  (contains? stm state))

(defn get-state-mapping [stm state]
  (when stm (get stm state)))


(defn get-transitions [stm state]
  (when stm
    (dissoc 
      (get (get-state-mapping stm state) :dreamcatcher/transitions)
      any-state)))


(defn get-states
  "Function returns valid states of STM.
  Returned states do not include any-state state
  since it is not deterministic."
  [stm]
  (-> stm keys set (disj any-state)))


(defn only-states 
  "Function returns vector of states. Should be used insied of defstm"
  [& states]
  (vector states))


(defn states-other-than 
  "Function takes value from *stm-constructor*. It will return function 
  that will return vector of states
  other than input states that are available throught *stm-constructor*"
  [& states]
  (fn []
    (let [states' (set (get-states @*stm-constructor*))]
      (vec (remove (set states) states')))))


(def except-states states-other-than)


(defn has-transition? [stm from-state to-state]
  (fn? (-> stm
           (get from-state)
           :dreamcatcher/transitions
           (get to-state))))


(defn get-transition [stm from-state to-state]
  (when (has-transition? stm from-state to-state)
    (-> stm (get from-state) :dreamcatcher/transitions (get to-state))))


(defn get-validators [stm state]
  (:dreamcatcher/validators (get-state-mapping stm state)))


(defn get-validator [stm state state']
  (get (get-validators stm state) state'))

;; State Machine is a simple map...
;; That contains transitons from one state to another.
;; It can also contain validators that evaluate if
;; transition is valid to happen or not.


;; StateMachine generation
(defn add-state [stm state]
  ; (assert (not (#{:state :data :stm} state)) "[:state :data :stm] are special keys")
  (swap! stm assoc state nil))


(defn remove-state [stm state]
  (swap! stm #(dissoc % state)))


(add-statemachine-mapping validator  :dreamcatcher/validators)
(add-statemachine-mapping transition :dreamcatcher/transitions)


(defn make-state-machine
  "Input values transitions and validators are ment to
  be sequences with recuring pattern

  [from-state to-state function from-state to-state... to-state function]

  \"function\" takes one argument and that is state
  machine instance."
  ([{stm-name :name
     :keys [transitions validators]
     :or {stm-name (str (gensym "dreamcatcher/stm_"))}}]
   (let [stm (atom (with-meta 
                     {}
                     {:dreamcatcher/stm stm-name 
                      :dreamcatcher/transitions [] 
                      :dreamcatcher/validators []}))
         t (partition 3 transitions)
         v (partition 3 validators)
         states (remove fn? (-> (map #(take 2 %) t) flatten set))]
     (binding [*stm-constructor* stm]
       (doseq [x states] (add-state stm x))
       (doseq [x t] (apply add-transition (conj x stm)))
       (doseq [x v] (apply add-validator (conj x stm)))
       @stm))))

(defmacro defstm
  "Macro defines stm with make-state-machine function.
  STM is persistent hash-map."
  [stm-name & [transitions validators]]
  `(def ~stm-name 
     (let [name# "mirko"] 
       (make-state-machine 
         (cond-> {}
           (some? ~stm-name) (assoc :name ~(str *ns* "/" stm-name))
           (some? ~transitions) (assoc :transitions ~transitions)
           (some? ~validators) (assoc :validators ~validators))))))


(defn join-stms 
  "Function joins multiple STMs into one single STM."
  [stm-name & stms]
  (let [transitions (reduce concat '() (map (comp :dreamcatcher/transitions meta) (concat [stm-name] stms)))
        validators (reduce concat '() (map (comp :dreamcatcher/validators meta) (concat [stm-name] stms)))]
    (make-state-machine 
      {:name stm-name 
       :transitions transitions 
       :validators validators})))


(defmacro composed-stm 
  [stm-name & stms]
  {:pre [(symbol? stm-name)]}
  `(def ~stm-name
     (join-stms ~(str *ns* "/" stm-name) ~@stms)))


;; Machine instance is map that represents
;; "real-machine" that has real state and real data
;; and has transitions defined in stm input argument
;;
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


(def instance-has-state? 
  "Returns true if"
  (comp has-state? stm))


(defn valid-transition?
  "Computes if transition from-state to to-state is valid.
  If there is any type of validator function than function
  result decides if transition is valid. If no validator
  is provided than transition is valid.

  If validator is not a function, transition is not valid."
  [^STMInstance instance from-state to-state]
  (if-let [vf (get (:dreamcatcher/validators (get-state-mapping (stm instance) from-state)) to-state)]
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
   (make-machine-instance stm initial-state data nil))
  ([stm initial-state data context]
   (STMInstance. stm initial-state data context)))

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
        (remove any-state?))))


(defn- step [^STMInstance instance from-state to-state fun]
  (when-not (nil? instance)
    (when  (valid-transition? instance from-state to-state)
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
      (let [moment #?(:clj (System/currentTimeMillis)
                      :cljs (.getTime (js/Date.)))
            from-state (state instance)
            tfunction (or (get-transition stm from-state to-state) identity)
            in-fun (or
                     (get-transition stm any-state to-state)
                     identity)
            out-fun (or
                      (get-transition stm from-state any-state)
                      identity)]
        (if-let [new-instance (some-> instance
                                  (step from-state any-state out-fun)
                                  (step from-state to-state tfunction)
                                  (step any-state to-state in-fun))]
          (let [new-instance (assoc new-instance :state to-state)]
            ;; Add hook for aditional utils spying on instances
            ;; Gets called only when transition already happened
            ;; and doesn't affect new-instance
            (when-let [always (get-transition stm any-state any-state)]
              (when (fn? always)
                (always instance new-instance)))
            new-instance)
          instance)))
    (throw
      (ex-info 
        (str "There is no state machine configured for in: " instance)
        {:type :dreamcatcher/definition
         :instance instance}))))


(defn- get-choices
  ([^STMInstance x] (get-choices x (state x)))
  ([^STMInstance x state]
   (vec 
     (remove 
       any-state?
       (if-let [fix-choices (-> x context ::life (get state))]
         fix-choices
         (-> x stm (get-transitions state) keys))))))


(defn- get-valid-candidates
  ([^STMInstance x] (get-valid-candidates x (state x)))
  ([^STMInstance x state]
   (when-let [choices (not-empty (get-choices x state))]
     (when (valid-transition? x state any-state)
       (seq 
         (filter 
           #(and 
              (valid-transition? x state %)
              ;; TODO - think about necessity for x->any state validator check 
              ;; for candidates. State might change after transition attempt and
              ;; new data might be valid for x->any-state validator
              (valid-transition? x any-state %))
           choices))))))


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
     (let [stm (if direct? (dissoc stm any-state) stm)]
       (letfn [(visited? [path state]
                 (not= -1 (.indexOf #?(:clj path :cljs (clj->js path)) state)))
               (generate-paths [current-path]
                 (let [c (last current-path)]
                   (if-not (= c end)
                     (if-let [targets (seq
                                        (concat
                                          (-> stm (get-transitions (or c start)) keys)
                                          (when-not direct? (keys (get-transitions stm any-state)))))]
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
                                                (for [x (keys (dissoc (stm this) any-state))]
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
                             (let [next-x (try 
                                            (move x (first path-left))
                                            (catch #?(:clj Throwable :cljs js/Error) e
                                              (throw
                                                (ex-info 
                                                  "State couldn't be reached"
                                                  {:type :dreamcatcher/movement
                                                   :exception e
                                                   :instance x}))))]
                               (if (= (state next-x) c) nil
                                 (recur next-x (state next-x) (rest path-left))))
                             ))))]
       (if-let [instance' (some tranverse paths)]
         instance'
         (throw 
           (ex-info 
             (str "State couldn't be reached " state')
             instance)))))))


(defmacro multimove 
  "Move STMInstance through series of states"
  [instance & states]
  (let [movements# (map #(list `move %) states)]
    `(-> ~instance ~@movements#)))


(defn to->through 
  "Function returns paths that contain thgrough state"
  [instance target through?]
  (filter (partial some through?) (to-> stm target)))



;; Wrappers
(defn wrap-transitions 
  ([stm function] (wrap-transitions stm function :before))
  ([stm function position?]
   (let [states (remove #{any-state} (get-states stm))]
     (reduce
       (fn [stm state]
         (reduce
           (fn [stm [to transition]]
             (assoc-in stm 
               [state :dreamcatcher/transitions to]
               (case position? 
                 :before (comp transition function)
                 :after (comp function transition))))
           stm
           (get-transitions stm state)))
       stm
       states))))


(defn any-wrap-any 
  ([stm f] (any-wrap-any stm f :before))
  ([stm f position?]
   (update-in stm [any-state :dreamcatcher/transitions any-state] 
              (fn [transition] 
                (case position?
                  :after (comp f transition)
                  :before (comp transition f))))))

(defn add-state-history [instance]
  (update-data! 
    instance
    update 
    :dreamcatcher/state-history
    (fnil conj [])
    (state instance)))


(def get-state-history (comp :dreamcatcher/state-history data))


(defn wrap-transition-duration [stm]
  (letfn [(now []
            #?(:clj (System/currentTimeMillis)
               :cljs (.getTime (js/Date.))))
          (transition-start [instance]
            (update-data! instance assoc :dreamcatcher.transition/start (now)))
          (transition-end [instance]
            (let [{:keys [:dreamcatcher.transition/start]} (data instance)]
              (->
                instance
                (update-data! update :dreamcatcher/transition-duration 
                              (fnil conj []) {:start start :end (now)})
                (update-data! dissoc :dreamcatcher.transition/start))))] 
    (->
      stm
      (wrap-transitions transition-start :before)
      (wrap-transitions transition-end :after))))

(defn get-transition-duration [instance]
  (let [{:keys [:dreamcatcher/transition-duration
                :dreamcatcher/state-history]} (data instance)]
    (when (not-empty transition-duration)
      (reduce
        (fn [r [k {:keys [start end]}]] (assoc r k (- end start)))
        (array-map)
        (partition 2 
                   (interleave 
                     (map vector (butlast state-history) (rest state-history)) 
                     transition-duration))))))

(defn add-data-history [instance]
  (update-data! 
    instance
    update 
    :dreamcatcher/data-history
    (fn [history] (conj (or history []) (data instance)))))


(def get-data-history (comp :dreamcatcher/data-history data))



(comment
  (defstm fucker [1 2 identity
                  2 3 identity
                  3 4 identity])
  (def f (make-machine-instance 
           (-> fucker 
               (wrap-transitions add-state-history)
               wrap-transition-duration) 
           1 {:hell 'no}))
  (def f' (reach-state f 4)))
