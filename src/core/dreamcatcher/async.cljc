(ns dreamcatcher.async
  #?(:cljs (:require-macros 
             [cljs.core.async.macros :refer [go]]))
  #?(:clj
      (:require
        [dreamcatcher.core :as dreamcatcher
         :refer [move 
                 any-state
                 state-changed? 
                 get-states 
                 get-transitions]]
        [clojure.core.async :as async 
         :refer [mult 
                 mix 
                 chan 
                 tap 
                 admix 
                 close! 
                 put! 
                 take! 
                 go
                 promise-chan]])
     :cljs
     (:require
       [dreamcatcher.core :as dreamcatcher
        :refer [*move*
                move 
                any-state
                state-changed? 
                get-states 
                get-transitions]]
       [cljs.core.async :as async 
        :refer [mult mix chan tap 
                admix close! put! take!]])))


(defprotocol AsyncSTMData
  (in-mix 
    [this]
    "Function returns map with states as keys, and mix objects as vals. All states
     are mix/mult so it is possible to tap and admix any one of state channels.")
  (out-mult 
    [this] 
    "Function returns map with states as keys, and mult objects as vals. All states
     are mix/mult so it is possible to tap and admix any one of state channels.")
  (state-channels [this] "Function returns map with {state state-channel} mapping.")
  (get-state-channel [this state] "Function returns target state channel")
  (transition-channels 
    [this] 
    "Function returns transitions mapping. First level are source states,
     afterward is destination state and transition-channel that is transduced
     with STM transition."))


(defprotocol AsyncSTMIO
  (inject 
    [this state data] 
    "Injects data to state channel.")
  (suck
    [this state]
    [this state buffer-size]
    [this state buffer-size xf]
    [this state buffer-size xf exh] 
    "Creates a channel that taps into state channel with input params.")
  (penetrate [this state channel] "Taps input channel to input state."))


(defprotocol AsyncSTMControl
  (disable 
    [this] 
    "Function closes all state channels, hence all transition channels are closed."))


(defn wrap-async-machine
  "Function refied async representation of STM instance
  that is interconnected with channels. States are represented with
  channels."
  [stm & {:keys [exception-fn 
                 move-exception-handler
                 penetration-fn 
                 parallel-penetrations]
          :or {exception-fn (fn [_] nil)
               move-exception-handler (fn [instance next-state]
                                        (throw 
                                          (ex-info 
                                            (str "Couldn't transition to state " next-state)
                                            {:type :dreamcatcher/movement 
                                             :instance instance
                                             :next-state next-state})))
               parallel-penetrations 1
               penetration-fn  (fn [stm state]
                                 ;; Wrapped for penetrating
                                 ;; To enable custom penetrations in future...
                                 ;; Returned function is of one argument and should
                                 ;; return machine instance.
                                 (fn [received-stm-instance] 
                                   (dreamcatcher/make-machine-instance 
                                     stm state 
                                     (dreamcatcher/data received-stm-instance))))}}]
  (let [states (get-states stm)
        channeled-states (reduce 
                           (fn [channel-map channel-name] 
                             (assoc channel-map channel-name (chan))) 
                           {} 
                           states)
        mult-states (reduce 
                      (fn [mult-map state] 
                        (assoc mult-map state (mult (get channeled-states state)))) 
                      {} 
                      states)
        ;; TODO - decide if states with no output transition should be "mult/ed" or not
        ;; At the moment they are!
        mix-states (reduce 
                     (fn [mix-map state] 
                       (assoc mix-map state (mix (get channeled-states state))))
                     {} 
                     states)
        channel-move (fn [next-state]
                       (fn [instance]
                         (let [temp-state (move instance next-state)]
                           (if (state-changed? temp-state instance)
                             temp-state
                             (move-exception-handler instance next-state)))))
        transition-channels (reduce 
                              merge
                              (for [s states]
                                (when-not (= any-state s)
                                  {s (reduce 
                                       merge
                                       (for [transition (get-transitions stm s)
                                             :let [to-state (key transition)]]
                                         (when-not (= any-state to-state)
                                           (let [tf (channel-move to-state)]
                                             ;; TODO - Error fn for logging or stuf would be better 
                                             ;; than just retruning nil
                                             {to-state (chan 1 (map tf) exception-fn)}))))})))
        map-transition (fn [s transition]
                         (when-not (or (= any-state s) (= any-state (key transition)))
                           (let [target-state (key transition)
                                 transition-channel (val transition)]
                             ;; Tap transition to source-state
                             (when transition-channel
                               (tap (get mult-states s) transition-channel))
                             ;; And add output from transition channel to target state
                             (when transition-channel
                               (admix (get mix-states target-state) transition-channel)))))]
    ;; Make connections between states through transition-channels
    (doseq [s states transition (get transition-channels s)]
      (map-transition s transition))
    ;transition-channels
    (reify
      AsyncSTMData
      (in-mix [_] mix-states)
      (out-mult [_] mult-states)
      (state-channels [_] channeled-states)
      (get-state-channel [_ state] (get channeled-states state))
      (transition-channels [_] transition-channels)
      AsyncSTMIO
      (inject [this state data]
        (async/put! 
          (get-state-channel this state)
          (dreamcatcher/make-machine-instance stm state data)))
      (suck [this state]
        (suck this state 1))
      (suck [this state buffer-size]
        (let [x (chan buffer-size)]
          (tap (get (out-mult this) state) x)
          x))
      (suck [this state buffer-size xf]
        (let [x (chan buffer-size xf)]
          (tap (get (out-mult this) state) x)
          x))
      (suck [this state buffer-size xf ex]
        (let [x (chan buffer-size xf ex)]
          (tap (get (out-mult this) state) x)
          x))
      (penetrate [this state channel]
        (let [pipeline-transducer (penetration-fn stm state)]
          (async/pipeline 
            parallel-penetrations
            (get-state-channel this state)
            ;; When penetrating it is assumed that input channel is
            ;; taken(sucked) from other dreamcatcher STM. So for this
            ;; STM model new instance must be created with data that is
            ;; Received from input channel. Hence:
            (map pipeline-transducer) channel)))
      AsyncSTMControl
      (disable [this]
        (doseq [x (vals (state-channels this))]
          (close! x))))))

;; Additional utils
(defn ground-channel [channel]
  (go
    (while (not (nil? (async/<! channel))))))


(defmacro promise-value [& body]
  `(let [promise_holder# (clojure.core.async/promise-chan)]
     (go
       (let [value# ~@body]
         (clojure.core.async/put! 
           promise_holder# value#)))
    promise_holder#))


(defn- step [instance from-state to-state fun]
  (when-not (nil? instance)
    (promise-value
      (let [instance' (async/<! instance)]
        (when (dreamcatcher/valid-transition? instance' from-state to-state)
          (let [new-state (fun instance')]
            (assert 
              (satisfies? dreamcatcher/STM instance)
              (str "Input fun takes old STM instance and produces new STM instance. Please asure that fn: " fun " returns implentation of STM protocol."))
            new-state))))))


; (defn- ^:no-doc move-stm
;   [instance to-state]
;   (promise-value 
;     (let [instance (async/<! instance)] 
;       (if-let [stm (stm instance)]
;         (do
;           (when-not (contains? stm to-state)
;             (ex-info 
;               (str "STM doesn't contain state " to-state)
;               {:type :dreamcatcher/movement
;                :instance instance
;                :to-state to-state}))
;           (when-not (has-transition? stm (state instance) to-state)
;             (ex-info
;               (str "There is no transition function from state " (state instance) " to state " to-state)
;               {:type :dreamcatcher/movement
;                :instance instance
;                :to-state to-state}))
;           (let [moment #?(:clj (System/currentTimeMillis)
;                           :cljs (.getTime (js/Date.)))
;                 from-state (state instance)
;                 tfunction (or (get-transition stm from-state to-state) identity)
;                 in-fun (or
;                          (get-transition stm any-state to-state)
;                          identity)
;                 out-fun (or
;                           (get-transition stm from-state any-state)
;                           identity)]
;             (if-let [new-instance (some-> instance
;                                           (step from-state any-state out-fun)
;                                           (step from-state to-state tfunction)
;                                           (step any-state to-state in-fun))]
;               (let [new-instance (if (not= (state instance) (state new-instance)) 
;                                    new-instance
;                                    (assoc new-instance :state to-state))]
;                 ;; Add hook for aditional utils spying on instances
;                 ;; Gets called only when transition already happened
;                 ;; and doesn't affect new-instance
;                 (when-let [always (get-transition stm any-state any-state)]
;                   (when (fn? always)
;                     (always instance new-instance)))
;                 new-instance)
;               instance)))
;         (throw
;           (ex-info 
;             (str "There is no state machine configured for in: " instance)
;             {:type :dreamcatcher/definition
;              :instance instance}))))))

(extend-type clojure.core.async.impl.channels.ManyToManyChannel
  dreamcatcher/STM
  (dreamcatcher/stm [this] (promise-value (dreamcatcher/stm (async/<! this))))
  (dreamcatcher/state [this] (promise-value (dreamcatcher/state (async/<! this))))
  (dreamcatcher/set-state! [this state'] (promise-value (dreamcatcher/set-state! (async/<! this) state')))
  (dreamcatcher/set-context! [this context'] (promise-value (assoc (async/<! this) :context context')))
  (dreamcatcher/set-data! [this data'] (promise-value (assoc (async/<! this) :data data')))
  (dreamcatcher/set-stm! [this stm'] (promise-value (assoc (async/<! this) :stm stm')))
  (dreamcatcher/data [this] (promise-value (dreamcatcher/data (async/<! this))))
  (dreamcatcher/context [this] (promise-value (dreamcatcher/context (async/<! this))))
  dreamcatcher/STMMovement
  (choices? [this] (promise-value (dreamcatcher/choices? (async/<! this))))
  (candidates? [this] (promise-value (dreamcatcher/candidates? (async/<! this))))
  (move [this next-state] )
  ; dreamcatcher/STMLife
  ; (give-life!
  ;   ([this choices] 
  ;    (promise-value
  ;      (let [this (async/<! this)
  ;            proposed-life (reduce conj (array-map)
  ;                                  (for [x (keys (dissoc (stm this) dreamcatcher/any-state))]
  ;                                    [x (dreamcatcher/get-reachable-states this x)]))]
  ;        (cond-> (dreamcatcher/update-context! this assoc ::alive? true)
  ;          (seq proposed-life) (update-context! assoc ::life proposed-life)
  ;          (seq choices) (update-context! update ::life merge choices)))))
  ;   ([this] (give-life! this nil)))
  ; (alive? [this] (get (context this) ::alive?))
  ; (kill! [this] (promise-value (update-context! this assoc ::alive? false)))
  ; (act!
  ;   ([this]
  ;    (assert (alive? this) "Instance is not alive! First give it life...")
  ;    nil))
  ; dreamcatcher/STMPath
  ; (to->
  ;   ([this target]
  ;    (dreamcatcher/from->to (dreamcatcher/stm this) (dreamcatcher/state this) target true)))
  ; (reach-state
  ;   ([instance state']
  ;    (let [stm (stm instance)
  ;          paths (to-> instance state')
  ;          move (memoize move) ;; Do not apply transitions more than once per try
  ;          tranverse (fn [path]
  ;                      (loop [x instance
  ;                             c (state instance)
  ;                             path-left path]
  ;                        (if (= c state') x
  ;                          (if-not (seq path-left) x
  ;                            (let [next-x (move x (first path-left))]
  ;                              (if (= (state next-x) c) x
  ;                                (recur next-x (state next-x) (rest path-left))))))))
  ;          traversed-paths (map tranverse paths)
  ;          state-reached? (comp (partial = state') state)]
  ;      (if-let [instance' (some
  ;                           (fn [instance] (when (state-reached? instance) instance)) 
  ;                           traversed-paths)]
  ;        instance'
  ;        (throw 
  ;          (ex-info 
  ;            (str "State couldn't be reached " state')
  ;            {:type :dreamcatcher/reach-state
  ;             :dreamcatcher/reached-states traversed-paths})))))))
  )

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
   (promise-value (dreamcatcher/->STMInstance stm initial-state data context))))
