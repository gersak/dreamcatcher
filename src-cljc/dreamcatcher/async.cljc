(ns dreamcatcher.async
  #?(:cljs (:require-macros [dreamcatcher.core :refer [defstm with-stm safe]]
                            [clojure.core.async :refer [go]]))
  #?(:clj
      (:require
        [dreamcatcher.core :refer [move state-changed? make-machine-instance data?]]
        [dreamcatcher.util :refer [get-states get-transitions]]
        [clojure.core.async :as async :refer [mult mix chan tap admix close! put! take! go]])
     :cljs
     (:require
       [dreamcatcher.core :refer [move state-changed? make-machine-instance data?]]
       [dreamcatcher.util :refer [get-states get-transitions]]
       [cljs.core.async :as async :refer [mult mix chan tap admix close! put! take!]])))


(defprotocol AsyncSTMData
  (in-mix [this]
           "Function returns map with states as keys, and mix objects as vals. All states
  are mix/mult so it is possible to tap and admix any one of state channels.")
  (out-mult [this] "Function returns map with states as keys, and mult objects as vals. All states
  are mix/mult so it is possible to tap and admix any one of state channels.")
  (state-channels [this] "Function returns map with {state state-channel} mapping.")
  (get-state-channel [this state] "Function returns target state channel")
  (transition-channels [this] "Function returns transitions mapping. First level are source states,
  afterward is destination state and transition-channel that is transduced
  with STM transition."))

(defprotocol AsyncSTMIO
  (inject [this state data] "Injects data to state channel.")
  (suck
    [this state]
    [this state buffer-size]
    [this state buffer-size xf]
    [this state buffer-size xf exh] "Creates a channel that taps into state channel with input params.")
  (penetrate [this state channel] "Taps input channel to input state."))

(defprotocol AsyncSTMControl
  (disable [this] "Function closes all state channels, hence all transition channels are closed."))


(defn wrap-async-machine
  "Function refied async representation of STM instance
  that is interconnected with channels. States are represented with
  channels."
  [stm & {:keys [exception-fn]
          :or [exception-fn (fn [_] nil)]}]
  (let [states (get-states stm)
        channeled-states (reduce (fn [channel-map channel-name] (assoc channel-map channel-name (chan))) {} states)
        mult-states (reduce (fn [mult-map state] (assoc mult-map state (mult (get channeled-states state)))) {} states)
        ;; TODO - decide if states with no output transition should be "mult/ed" or not
        mix-states (reduce (fn [mix-map state] (assoc mix-map state (mix (get channeled-states state)))) {} states)
        channel-move (fn [next-state]
                       (fn [instance]
                         (let [temp-state (move instance next-state)]
                           (when (state-changed? temp-state instance)
                             temp-state))))
        transition-channels (reduce merge
                                    (for [s states]
                                      (when-not (#{:any} s)
                                        {s (reduce merge
                                                   (for [transition (get-transitions stm s)
                                                         :let [to-state (key transition)]]
                                                     (when-not (#{:any} to-state)
                                                       (let [tf (channel-move to-state)]
                                                         ;; TODO - Error fn for logging or stuf would be better 
                                                         ;; than just retruning nil
                                                         {to-state (chan 1 (map tf) exception-fn)}))))})))
        map-transition (fn [s transition]
                         (when-not (or (#{:any} s) (#{:any} (key transition)))
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
        (async/put! (get-state-channel this state) (make-machine-instance stm state data)))
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
        (async/pipeline 
          1 
          (get-state-channel this state)
          (map #(make-machine-instance stm state (data? %))) channel))
      AsyncSTMControl
      (disable [this]
        (doseq [x (vals (state-channels this))]
          (close! x))))))

;; Additional utils
(defn ground-channel [channel]
  (go
    (while (not (nil? (async/<! channel))))))
