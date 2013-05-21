(in-ns 'baloime)

(def ^:private blank-job-machine (make-state-machine [:initialize :start (fn [x] (assoc-data x "*started-at*" (t/now)))
                                                      :start :end identity
                                                      :end :finished (fn [x] (assoc-data x "*ended-at*" (t/now)))]))

(defn job-life [x]
  (if (or (= :finished (get-state x)) (= false (-> x get-data (get "*running*"))))
    x
    (do
      (send-off *agent* #'job-life)
      (act! x))))

(defn- get-next-phase [job phase]
  (let [t (-> job phase :transitions)] 
    (when (seq t) (-> t first key))))

(defn ^:private get-job-phases [job]
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
  ;;([^clojure.lang.Atom job new-phase at-phase function] (insert-phase job new-phase at-phase function nil))
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
  (current-phase [this] "Returns current phase that job-agent is working on")
  (get-phases [this] "Lists all Job phases")
  (started-at? [this] "Returns org.joda.time.DateTime timestamp")
  (ended-at? [this] "Returns org.joda.time.DateTime timestamp")
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
  (reset-job! [this] "Returns job to initialized state."))



(defrecord Job [job-agent]
  JobInfo
  (get-phases [this] (-> @job-agent get-stm get-job-phases))
  (current-phase [this] (or 
                          (get-next-phase (get-stm @job-agent) (get-state @job-agent))
                          (get-state @job-agent)))
  (started-at? [this] (-> @job-agent get-data (get "*started-at*")))
  (ended-at? [this] (-> @job-agent get-data (get "*ended-at*")))
  (finished? [this] (= :finished (get-state  @job-agent)))
  (duration? [this] (if (.finished? this)
                      (-> (t/interval (.started-at? this) (.ended-at? this)) t/in-msecs)))
  (in-error? [this] (agent-error job-agent))
  (active? [this] (-> @job-agent get-data (get "*running*") (not= false)))
  JobActions
  (start! [this] (do
                   (send job-agent #(assoc-data % "*running*" true))
                   (await job-agent)
                   (send job-agent job-life)))
  (stop! [this] (do
                  (send job-agent #(assoc-data % "*running*" false))
                  (await job-agent)))
  (reset-job! [this] 
    (if (agent-error job-agent)
      (restart-agent job-agent (-> @job-agent 
                                   (remove-data (-> @job-agent get-data keys))
                                   (reset-state! :initialize)))
      (do
        (.stop! this)
        (send job-agent (fn [x] (-> x 
                                    (clear-data)
                                    (reset-state! :initialize))))
        (await job-agent)))))


(defn make-job [phases]
  (let [job (atom blank-job-machine)]
    (loop [phases phases]
      (if (empty? phases) 
        (Job. (agent (-> (get-machine-instance @job :initialize)
                         give-life!)))
        (recur (do
                 (add-phase job (first phases) (take-while fn? (rest phases)))
                 (drop-while fn? (rest phases))))))))

(defmacro defjob 
  "Defines Job record that successively executes
  functions that are defined as transitions from
  phase to phase. Transitions are valid if validator
  function returns true or if validator is not defined."
  [name & phases]
  `(def ~name (let [job# (atom ~blank-job-machine)]
                (loop [phases# ~@phases]
                  (if (empty? phases#) 
                    (Job. (agent (-> (get-machine-instance @job# :initialize)
                                     give-life!)))
                    (recur (do
                             (add-phase job# (first phases#) (take-while fn? (rest phases#)))
                             (drop-while fn? (rest phases#)))))))))



(defjob test-job [:test1 (fn [x] (println "Testis 1") x) (fn [_] (Thread/sleep 5000) true)
                  :test2 (fn [x] (println "Testis 2") x) (fn [_] (Thread/sleep 3000) true)
                  :test3 (fn [x] (println "Testis 3") x) (fn [_] (Thread/sleep 1000) (println "hanging") false)])



















;; Tests

;;(def test-job (make-job :test-job 
;;                        :test1 (fn [x] (println "Testis 1") x) (fn [_] (Thread/sleep 5000) true)
;;                        :test2 (fn [x] (println "Testis 2") x) (fn [_] (Thread/sleep 3000) true)
;;                        :test3 (fn [x] (println "Testis 3") x) (fn [_] (Thread/sleep 1000) (println "hanging") false)))

;;(def test-error (make-job :test-job 
;;                          :test1 (fn [x] (println "Testis 1") x) (fn [_] (Thread/sleep 5000) true)
;;                          :test2 (fn [x] (throw (Exception. "Testing error")) x) (fn [_] (Thread/sleep 2000) true)
;;                          :test3 (fn [x] (println "Testis 3") x) (fn [_] (Thread/sleep 3000) true)))
;;

(defn test-insert []
  (let [machine (insert-phase test-job :test1.5 :test1 (fn [x] (println "Test broj 1.5") x) (fn [_] true))

        a (agent (-> (get-machine-instance machine :start) give-life!))]
    (send-off a job-life)
    a))

(defn test-remove []
  (let [machine (insert-phase test-job :test1.5 :test1 (fn [x] (println "Test broj 1.5") x) (fn [_] true))
        machine (remove-phase machine :test1.5)

        a (agent (-> (get-machine-instance machine :start) give-life!))]
    (send-off a job-life)
    a))
