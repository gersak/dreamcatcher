(ns baloime.jobs
  (:use [dreamcatcher.core :reload-all true]
        [dreamcatcher.util :reload-all true]
        [clj-time.local :only (local-now)])
  (:require [clj-time.core :as t]))

;;(in-ns 'baloime)

(def ^:private blank-job-machine (make-state-machine [:initialize :start (fn [x] (assoc-data x "*started-at*" (local-now)))
                                                      :start :end identity
                                                      :end :finished (fn [x] (assoc-data x "*ended-at*" (local-now)))]))

(def ^:dynamic *jobs* (ref {}))


;;(defn job-life [x]
;;  (if (get @*jobs* x) 
;;    (if (:finished (get-state x))
;;      (dosync
;;        (alter *jobs* assoc x false)
;;        x)
;;      (doall
;;        (send-off *agent* #'job-life)
;;        (act! x)))
;;    x))


(defn job-life [x]
  (if (or (= :finished (get-state x)) (= false (get @*jobs* x)))
    x
    (do
      (send-off *agent* #'job-life)
      (act! x))))

(defn- get-next-phase [job phase]
  (let [t (-> job phase :transitions)] 
    (when (seq t) (-> t first key))))

(defn- get-job-phases [job]
  (loop [phase :start
         phases nil]
    (if (= phase :end) (-> phases reverse rest) 
      (recur (get-next-phase job phase) (conj phases phase)))))

(defn- get-previous-phase [job phase]
  (or (first (filter #(has-transition? job % phase) (get-job-phases job))) :start))

(defn add-phase 
  "Functions adds phase to the end
  of the phase chain that completes
  job"
  ([^clojure.lang.Atom job new-phase [function validator]]
   (assert (not-any? #(= % new-phase) (get-states @job)) "Phase already defined")
   (let [last-phase (first (filter #(has-transition? @job % :end) (get-states @job)))]
     (remove-transition job last-phase :end)
     (remove-validator job last-phase :end)
     (add-state job new-phase)
     (add-transition job last-phase new-phase function)
     (when validator (add-validator job last-phase new-phase validator))
     (add-transition job new-phase :end identity)
     job)))

(defn insert-phase 
  "Inserts new phase after at-phase"
  ([^clojure.lang.Atom job new-phase at-phase function validator]
   (assert (not-any? #(= % new-phase) (get-states @job)) "Phase already defined")
   (let [next-phase (get-next-phase @job  at-phase)
         transition (get-transition @job at-phase next-phase)
         p-validator (first (get-validators @job at-phase))]
     (remove-transition job at-phase next-phase)
     (remove-validator job at-phase next-phase)
     (add-state job new-phase)
     (add-transition job at-phase new-phase function)
     (add-transition job new-phase next-phase transition)
     (when validator (add-validator job new-phase next-phase validator))
     (when p-validator (add-validator job at-phase new-phase validator)) 
     job)))

(defn remove-phase
  "Removes phase from job"
  [^clojure.lang.Atom job phase]
  (assert (some #(= % phase) (get-states @job)) "Phase is not defined for this job")
  (let [next-phase (get-next-phase @job phase)
        previous-phase (get-previous-phase @job phase)
        tp (get-transition @job phase next-phase)
        vp (first (get-validators @job phase))]
    (remove-transition job previous-phase phase)
    (remove-validator job previous-phase phase)
    (remove-state job phase)
    (add-transition job previous-phase next-phase tp)
    (add-validator job previous-phase next-phase vp)
    job))

(defprotocol JobInfo
  (at-phase? [this] "Returns current phase that job-agent is working on")
  (before-phase? [this phase] "Returns boolean true if current phase that job-agent is working on is before input phase")
  (after-phase? [this phase] "Returns boolean true if current phase that job-agent is working on is before input phase")
  (get-phases [this] "Lists all Job phases")
  (started-at? [this] "Returns org.joda.time.DateTime timestamp")
  (ended-at? [this] "Returns org.joda.time.DateTime timestamp")
  (started? [thsi] "Returns true if job is not in phase start")
  (finished? [this] "Returns true is job is in :finished state")
  (active? [this] "Returns true if job is running")
  (duration? [this] "Returns duration of job in milliseconds")
  (in-error? [this] "Returns error exception if it happend. Otherwise nil"))

(defprotocol JobActions
  (start! [this] "Starts Job. That is sends-off job-agent to do the job. If Job
                 was previously stoped than it continues from last phase.")
  (stop! [this] "Stops running Job. If Job will allways try to complete current phase.
                If validator doesn't allow execution to continue than Job is stoped
                at current phase.")
  (reset-job! [this] [this params] "Returns job to initialized state. Optional parameter
                                   params is for data initialization."))



(defrecord Job [job-agent]
  JobInfo
  (get-phases [this] (-> @job-agent get-stm get-job-phases))
  (at-phase? [this] (or 
                      (get-next-phase (get-stm @job-agent) (get-state @job-agent))
                      (get-state @job-agent)))
  (before-phase? [this phase] (let [phases (.get-phases this)
                                    current (.at-phase? this)]
                                (or 
                                  (boolean (#{:start :initialize} current))
                                  (< (.indexOf phases current) (.indexOf phases phase)))))
  (after-phase? [this phase] (let [phases (.get-phases this)
                                   current (.at-phase? this)]
                               (or 
                                 (boolean (#{:finished :end} current))
                                 (> (.indexOf phases current) (.indexOf phases phase)))))
  (started-at? [this] (-> @job-agent get-data (get "*started-at*")))
  (ended-at? [this] (-> @job-agent get-data (get "*ended-at*")))
  (started? [this] (not= :start (.at-phase? this)))
  (finished? [this] (= :finished (get-state  @job-agent)))
  (duration? [this] (if (.finished? this)
                      (-> (t/interval (.started-at? this) (.ended-at? this)) t/in-msecs)))
  (in-error? [this] (agent-error job-agent))
  (active? [this] (get @*jobs* this))
  JobActions
  (start! [this] (dosync
                   (alter *jobs* assoc this true)
                   (send-off job-agent job-life)))
  (stop! [this] (dosync (alter *jobs* assoc this false)))
  (reset-job! [this] (.reset-job! this nil))
  (reset-job! [this params] 
    (if (agent-error job-agent)
      (do
        (.stop! this)
        (restart-agent job-agent (-> @job-agent 
                                     (remove-data (-> @job-agent get-data keys))
                                     (reset-state! :initialize))))
      (letfn [(initialize-params [x params]
                (assoc x :data params :state :initialize))]
        (.stop! this)
        (send-off job-agent initialize-params params)))))


(defn make-job-shell [phases]
  (let [job (atom blank-job-machine)]
    (loop [phases phases]
      (if (empty? phases) 
        (Job. (agent (-> (get-machine-instance @job :initialize)
                         give-life!)))
        (recur (do
                 (add-phase job (first phases) (take-while fn? (rest phases)))
                 (drop-while fn? (rest phases))))))))


(defn job
  "Returns real Job record with life. It
  is possible to initialize data for job.
  Pass in params after job definition."
  ([j] (Job. (-> j (get-machine-instance :initialize) give-life! agent)))
  ([j & params] (let [initialize-data (fn [x] (apply assoc-data (conj params x)))]
                  (Job. (-> j 
                            (get-machine-instance :initialize) 
                            initialize-data
                            give-life!
                            agent)))))

(defn register-job [job]
  (dosync
    (alter *jobs* assoc job false)
    job))

(defn unregister-job [job]
  (dosync
    (alter *jobs* dissoc job)
    job))

(defmacro defjob 
  "Defines job shell that represents blueprint for
  successively executing functions that are defined 
  as transitions from phase to phase. Transitions are
  valid if validator function returns true or if 
  validator is not defined."
  [name & phases]
  `(def ~name (let [job# (atom ~blank-job-machine)
                    job-shell# (loop [phases# ~@phases]
                                 (if (empty? phases#) 
                                   @job#
                                   (recur (do
                                            (add-phase job# (first phases#) (take-while fn? (rest phases#)))
                                            (drop-while fn? (rest phases#))))))]
                (register-job (Job. (-> job-shell#
                                        (get-machine-instance :initialize)
                                        give-life!
                                        agent))))))

;; Reason for this macro is if "I" don't wan't to make
;; chage to Job. data durring Job. phases than this 
;; macro evaluates body while returning Job. data as it was.
(defmacro safe
  "Simple wrapping macro for easier
  defjob definition. Wraps body in a 
  function THAT returns same argument 
  that was argument. Body parts are 
  evaluated thorougly."
  [& body]
  `(fn [x#]
     (do ~@body) x#))

(defn depend-on 
  "depend-on function is wrapper for job
  validator, that ties execution of next
  phase to successfull finish of other
  jobs."
  [check-timer & jobs] (fn [_] (Thread/sleep check-timer) (every? finished? jobs)))

(defn start-jobs [& jobs] (fn [x] (doseq [j jobs] (start! j)) x))

(defn stop-jobs [& jobs] (fn [x] (doseq [j jobs] (stop! j)) x))

(defn wait-for 
  "Wait for is another wrapper that can 
  be used as validator for job. It periodicaly
  checks if condition fn or condition is valid
  and. If condition is not provided than it is true."
  ([check-timer] (fn [_] (do (Thread/sleep check-timer) true)))
  ([check-timer condition] (fn [_] (Thread/sleep check-timer) (if (fn? condition) (condition) condition))))
