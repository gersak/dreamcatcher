(ns dreamcatcher.async
  (:require [dreamcatcher.macros :as m]
            [dreamcatcher.util :refer [get-states get-transitions]]
            ;; Only testing
            #?(:clj [dreamcatcher.core :refer [defstm]])
            [dreamcatcher.core :refer [defstm safe move state-changed? make-machine-instance data? state? make-machine-instance]]
            [clojure.core.async :as async :refer [mult mix chan tap admix close! put! take!]]))


(defprotocol AsyncSTMData
  (in-mix? [this]
           "Function returns map with states as keys, and mix objects as vals. All states
  are mix/mult so it is possible to tap and admix any one of state channels.")
  (out-mult? [this] "Function returns map with states as keys, and mult objects as vals. All states
  are mix/mult so it is possible to tap and admix any one of state channels.")
  (state-channels? [this] "Function returns map with {state state-channel} mapping.")
  (transition-channels? [this] "Function returns transitions mapping. First level are source states,
  afterward is destination state and transition-channel that is transduced
  with STM transition."))

(defprotocol AsyncSTMIO
  (inject [this state data] "Injects data to state channel.")
  (suck [this state]
        [this state buffer-size]
        [this state buffer-size xf]
        [this state buffer-size xf exh]))

(defprotocol AsyncSTMControl
  (disable [this] "Function closes all state channels, hence all transition channels are closed."))


(defn wrap-async-machine
  "Function refied async representation of STM instance
  that is interconnected with channels. States are represented with
  channels."
  [stm]
  (let [states (get-states stm)
        channeled-states (reduce (fn [channel-map channel-name] (assoc channel-map channel-name (chan))) {} states)
        mult-states  (reduce (fn [mult-map state] (assoc mult-map state (mult (get channeled-states state)))) {} states)
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
                                                         {to-state (chan 1 (map tf) (fn [_] nil))}))))})))
        map-transition (fn [s transition]
                         (when-not (or (#{:any} s) (#{:any} (key transition)))
                           (let [target-state (key transition)
                                 transition-channel (val transition)]
                             ;(println "Making connection: " s " -> " target-state)
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
      (in-mix? [_] mix-states)
      (out-mult? [_] mult-states)
      (state-channels? [_] channeled-states)
      (transition-channels? [_] transition-channels)
      AsyncSTMIO
      (inject [this state data]
        (async/put! (get (state-channels? this) state) (make-machine-instance stm state data)))
      (suck [this state] (suck this state 1))
      (suck [this state buffer-size]
        (let [x (chan buffer-size)]
          (tap (get (out-mult? this) state) x)
          x))
      (suck [this state buffer-size xf]
        (let [x (chan buffer-size xf)]
          (tap (get (out-mult? this) state) x)
          x))
      (suck [this state buffer-size xf ex]
        (let [x (chan buffer-size xf ex)]
          (tap (get (out-mult? this) state) x)
          x))
      AsyncSTMControl
      (disable [this]
        (doseq [x (vals (state-channels? this))]
          (close! x))))))




(letfn
  [(path-history [x]
     (println "Traversed  paths: " (conj (-> x data? :history) (state? x)))
     (-> x (update-in [:data :history] conj (state? x))))]
  (defstm simple-stm
    ;; Transitions
    [1 2 (safe (println "From 1->2"))
     2 3 (safe (println "From 2->3"))
     1 3 (safe (println "From 1->3"))
     3 [4 5] identity
     4 5 identity
     5 6 (safe (println "From 5->6"))
     #_(some identity
             ((juxt
                (with-stm x
                  (-> x
                      (assoc-in [:data :turbo] 1)))
                (with-stm x
                  (-> x
                      (assoc-in [:data :turbo] 100)))) %))
     ;:any 1 path-history]
     [1 2 3 4 5 6] :any path-history]
    ;; Validators
    [1 3 (fn [_] false)]))
