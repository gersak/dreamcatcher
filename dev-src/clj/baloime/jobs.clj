(in-ns 'baloime)

(def ^:private blank-job-machine (make-state-machine [:start :end identity]))

(defn- get-next-phase [job phase]
  (-> job phase :transitions first key))

(defn get-phases [job]
  (loop [phase :start
         phases nil]
    (if (= phase :end) (-> phases reverse rest) 
      (recur (get-next-phase job phase) (conj phases phase)))))

(defn- get-previous-phase [job phase]
  (first (filter #(has-transition? job % phase) (get-phases job))))

(defn add-phase 
  "Functions adds phase to the end
  of the phase chain that completes
  job"
  ([job new-phase function] (add-phase job new-phase function nil))
  ([job new-phase function validator]
   (assert (not-any? #(= % new-phase) (get-states job)) "Phase already defined")
   (let [last-phase (first (filter #(has-transition? job % :end) (get-states job)))
         job (atom job)]
     (remove-transition job last-phase :end)
     (remove-validator job last-phase :end)
     (add-state job new-phase)
     (add-transition job last-phase new-phase function)
     (when validator (add-validator job last-phase new-phase validator))
     (add-transition job new-phase :end identity)
     @job)))

(defn insert-phase 
  "Inserts new phase after at-phase"
  ([job new-phase at-phase function] (insert-phase job new-phase at-phase function nil))
  ([job new-phase at-phase function validator]
   (assert (not-any? #(= % new-phase) (get-states job)) "Phase already defined")
   (let [next-phase (get-next-phase job  at-phase)
         transition (get-transition job at-phase next-phase)
         p-validator (first (get-validators job at-phase))
         job (atom job)]
     (remove-transition job at-phase next-phase)
     (remove-validator job at-phase next-phase)
     (add-state job new-phase)
     (add-transition job at-phase new-phase function)
     (add-transition job new-phase next-phase transition)
     (when validator (add-validator job new-phase next-phase validator))
     (when p-validator (add-validator job at-phase new-phase validator)) 
     @job)))

(defn remove-phase
  "Removes phase from job"
  [job phase]
  (assert (some #(= % phase) (get-states job)) "Phase is not defined for this job")
  (let [next-phase (get-next-phase job phase)
        previous-phase (get-previous-phase job phase)
        tp (get-transition job phase next-phase)
        vp (first (get-validators job phase))
        job (atom job)]
    (remove-transition job previous-phase phase)
    (remove-validator job previous-phase phase)
    (remove-state job phase)
    (add-transition job previous-phase next-phase tp)
    (add-validator job previous-phase next-phase vp)
    @job))

;; Tests

(def test-job (-> blank-job-machine
                  (add-phase :test1 (fn [x] 
                                      (Thread/sleep 10000)
                                      (println "Test broj 1")
                                      x))
                  (add-phase :test2 (fn [x] 
                                      (Thread/sleep 10000)
                                      (println "Test broj 2") 
                                      x))))


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
