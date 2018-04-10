(ns dreamcatcher.async
  #?(:cljs (:require-macros 
             [cljs.core.async.macros :refer [go]]))
  #?(:clj
      (:require
        [dreamcatcher.core :as dreamcatcher
         :refer [move 
                 state-changed? 
                 make-machine-instance]]
        [dreamcatcher.util 
         :refer [get-states 
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
                 go]])
     :cljs
     (:require
       [dreamcatcher.core :as dreamcatcher
        :refer [move 
                state-changed? 
                make-machine-instance]]
       [dreamcatcher.util 
        :refer [get-states 
                get-transitions]]
       [cljs.core.async :as async 
        :refer [mult mix chan tap 
                admix close! put! take!]])))


(defprotocol AsyncSTMData
  (in-mix 
    [this]
    "Function returns map with states as keys, and mix objects as vals. All states
     are mix/mult so it is possible to tap and admix any one of state chanels.")
  (out-mult 
    [this] 
    "Function returns map with states as keys, and mult objects as vals. All states
     are mix/mult so it is possible to tap and admix any one of state chanels.")
  (state-chanels [this] "Function returns map with {state state-chanel} mapping.")
  (get-state-chanel [this state] "Function returns target state chanel")
  (transition-chanels 
    [this] 
    "Function returns transitions mapping. First level are source states,
     afterward is destination state and transition-chanel that is transduced
     with STM transition."))


(defprotocol AsyncSTMIO
  (inject 
    [this state data] 
    "Injects data to state chanel.")
  (suck
    [this state]
    [this state buffer-size]
    [this state buffer-size xf]
    [this state buffer-size xf exh] 
    "Creates a chanel that taps into state chanel with input params.")
  (penetrate [this state chanel] "Taps input chanel to input state."))


(defprotocol AsyncSTMControl
  (disable 
    [this] 
    "Function closes all state chanels, hence all transition chanels are closed."))


(defn wrap-async-machine
  "Function refied async representation of STM instance
  that is interconnected with chanels. States are represented with
  chanels."
  [stm & {:keys [exception-fn penetration-fn parallel-penetrations]
          :or {exception-fn (fn [_] nil)
               parallel-penetrations 1
               penetration-fn  (fn [stm state]
                                 ;; Wrapped for penetrating
                                 ;; To enable custom penetrations in future...
                                 ;; Returned function is of one argument and should
                                 ;; return machine instance.
                                 (fn [received-stm-instance] 
                                   (make-machine-instance 
                                     stm state 
                                     (dreamcatcher/data received-stm-instance))))}}]
  (let [states (get-states stm)
        chaneled-states (reduce 
                           (fn [chanel-map chanel-name] 
                             (assoc chanel-map chanel-name (chan))) 
                           {} 
                           states)
        mult-states (reduce 
                      (fn [mult-map state] 
                        (assoc mult-map state (mult (get chaneled-states state)))) 
                      {} 
                      states)
        ;; TODO - decide if states with no output transition should be "mult/ed" or not
        ;; At the moment they are!
        mix-states (reduce 
                     (fn [mix-map state] 
                       (assoc mix-map state (mix (get chaneled-states state))))
                     {} 
                     states)
        chanel-move (fn [next-state]
                       (fn [instance]
                         (let [temp-state (move instance next-state)]
                           (if (state-changed? temp-state instance)
                             temp-state
                             (throw 
                               (ex-info 
                                 (str "Couldn't transition to state " next-state)
                                 instance))))))
        transition-chanels (reduce 
                              merge
                              (for [s states]
                                (when-not (#{:any} s)
                                  {s (reduce 
                                       merge
                                       (for [transition (get-transitions stm s)
                                             :let [to-state (key transition)]]
                                         (when-not (#{:any} to-state)
                                           (let [tf (chanel-move to-state)]
                                             ;; TODO - Error fn for logging or stuf would be better 
                                             ;; than just retruning nil
                                             {to-state (chan 1 (map tf) exception-fn)}))))})))
        map-transition (fn [s transition]
                         (when-not (or (#{:any} s) (#{:any} (key transition)))
                           (let [target-state (key transition)
                                 transition-chanel (val transition)]
                             ;; Tap transition to source-state
                             (when transition-chanel
                               (tap (get mult-states s) transition-chanel))
                             ;; And add output from transition chanel to target state
                             (when transition-chanel
                               (admix (get mix-states target-state) transition-chanel)))))]
    ;; Make connections between states through transition-chanels
    (doseq [s states transition (get transition-chanels s)]
      (map-transition s transition))
    ;transition-chanels
    (reify
      AsyncSTMData
      (in-mix [_] mix-states)
      (out-mult [_] mult-states)
      (state-chanels [_] chaneled-states)
      (get-state-chanel [_ state] (get chaneled-states state))
      (transition-chanels [_] transition-chanels)
      AsyncSTMIO
      (inject [this state data]
        (async/put! 
          (get-state-chanel this state)
          (make-machine-instance stm state data)))
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
      (penetrate [this state chanel]
        (let [pipeline-transducer (penetration-fn stm state)]
          (async/pipeline 
            parallel-penetrations
            (get-state-chanel this state)
            ;; When penetrating it is assumed that input chanel is
            ;; taken(sucked) from other dreamcatcher STM. So for this
            ;; STM model new instance must be created with data that is
            ;; Received from input chanel. Hence:
            (map pipeline-transducer) chanel)))
      AsyncSTMControl
      (disable [this]
        (doseq [x (vals (state-chanels this))]
          (close! x))))))

;; Additional utils
(defn ground-chanel [chanel]
  (go
    (while (not (nil? (async/<! chanel))))))
