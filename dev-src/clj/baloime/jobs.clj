(in-ns 'baloime)

(def ^:private blank-job-machine (make-state-machine [:initialize :start (fn [x] (assoc-data x "*started-at*" (t/now)))
                                                      :start :end identity
                                                      :end :finished (fn [x] (assoc-data x "*ended-at*" (t/now)))]))

(defn job-life [x]
  (if (= :finished (get-state x)) x
    (do
      (send-off *agent* #'job-life)
      (act! x))))

(defn- get-next-phase [job phase]
  (-> job phase :transitions first key))

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
  (get-name [this])
  (start [this])
  (current-phase [this])
  (get-phases [this])
  (started-at? [this])
  (ended-at? [this])
  (finished? [this])
  (duration? [this]))


(defrecord Job [job-name job-agent]
  JobInfo
  (get-name [this] (identity job-name))
  (get-phases [this] (-> @job-agent get-stm get-job-phases ))
  (start [this] (send-off job-agent job-life))
  (current-phase [this] (get-state @job-agent))
  (started-at? [this] (-> @job-agent get-data (get "*started-at*")))
  (ended-at? [this] (-> @job-agent get-data (get "*ended-at*")))
  (finished? [this] (= :finished (get-state  @job-agent)))
  (duration? [this] (if (.finished? this)
                      (-> (t/interval (.started-at? this) (.ended-at? this)) t/in-msecs))))
;;



(defn make-job [name & phases]
  (let [job (atom blank-job-machine)]
    (loop [job job
           phases phases]
      (if (empty? phases) 
        (Job. name (agent (-> (get-machine-instance @job :initialize)
                              give-life!)))
        (recur (add-phase job (first phases) (take-while fn? (rest phases))) (drop-while fn? (rest phases)))))))


(def test-job (make-job :test-job 
                        :test1 (fn [x] (println "Testis 1") x) (fn [_] (Thread/sleep 1000) true)
                        :test2 (fn [x] (println "Testis 2") x)
                        :test3 (fn [x] (println "Testis 3") x)))


;;division by zero:
;;
;;(send bond (fn [x] (/ x 0)))(def bond (agent 7))
;;
;;(defn err-handler-fn [ag ex]
;;  (println "evil error occured: " ex " and we still have value " @ag))
;;
;;  (set-error-handler! bond err-handler-fn)
;;
;;  ;;division by zero:
;;  ;;
;;  ;;(send bond (fn [x] (/ x 0)))  kkkkkkkkkkkkkk

;; Tests

;;(def test-job @(-> (atom blank-job-machine)
;;                   (add-phase :test1 [(fn [x] 
;;                                        (Thread/sleep 10000)
;;                                        (println "Test broj 1")
;;                                        x)]
;;                    (add-phase :test2 [(fn [x] 
;;                                                   (Thread/sleep 10000)
;;                                                   (println "Test broj 2") 
;;                                                   x)]))))


(defn test-add []
  (let [machine test-job
        a (agent (-> (get-machine-instance machine :start) give-life!))]
    (send-off a job-life)
    (await-for  3000 a)
    @a))

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
    ;;(await a)
    a))
