(ns baloime.cron
  (:use dreamcatcher.core
        [clj-time.local :only (local-now to-local-date-time)])
  (:require [clj-time.core :as t]
            [clj-time.format :as tf]))

(defn cron-element-parserer 
  "Parses CRON like element. Elements are in form
  1-25/0
  1,5,40/10
  1,20,40
  20-40/5 
  */2 etc."
  [element]
  (let [[element interval] (clojure.string/split element #"/")
        temp (cond
               (= "*" element) nil 
               (seq (re-find #"-" element)) {:range (map #(Integer/valueOf %) (clojure.string/split element #"-"))}
               (seq (re-find #"," element)) {:sequence (apply sorted-set (map #(Integer/valueOf %) (clojure.string/split element #",")))}
               :else {:fixed (Integer/valueOf element)})]
    (if interval 
      (assoc temp :interval (Integer/valueOf interval))
      temp)))

(defn- set-constraints [cron-mapping]
  (let [trans [{:min 0 :max 59} ;; Seconds
               {:min 0 :max 59} ;; Minutes
               {:min 0 :max 23} ;; Hours
               {:min 1 :max 31} ;; Day of the month
               {:min 1 :max 12} ;; Month
               {:min 1 :max 7}  ;; Day of the week
               {:min 0 :max 4000}]
        prepared (partition 2 (interleave cron-mapping trans))]
    (map #(apply merge %) prepared)))

(defn- validate-range [x] (when (:range x)
                            (and (>= (-> x :range first) (:min x)) (<= (-> x :range second) (:max x)))))

(defn- validate-interval [x] (when (:interval x)
                               (and (<= (:interval x) (- (:max x) (:min x))))))

(defn- validate-sequence [x] (when (:sequence x)
                               (and (every? #(>= % (:min x)) (:sequence x)) (every? #(<= % (:max x)) (:sequence x)))))

(defn- validate-fixed [x] (when (:fixed x) (and (>= (:fixed x) (:min x)) (<= (:fixed x) (:max x)))))

(def ^:private validators [validate-range validate-sequence validate-fixed validate-interval])

(defn- validate-cron-mapping [x]
  (doseq [v validators] (assert (not (false? (v x))) (str (or (:range x) (:fixed x) (:sequence x) (:interval x)) " is out of range. [:min :max] = " [(:min x) (:max x)]))))

(defn parse-cron-string 
  "Parses CRON string e.g.

  \"0,3,20/20 0 0 3-20/10 * * *\" 

  If record is not valid assertion will
  be thrown. Returned data is sequence
  of cron-mappings that define what time
  is valid to execute Job."
  [^String cron-record]
  (let [elements (map clojure.string/trim (clojure.string/split cron-record  #" "))
        parts (set-constraints (map cron-element-parserer elements))]
    (assert (and (= (count parts) 7)) "Wrong number of elements passed in. Cron schedule has 7 elements.")
    (assert (>= 1 (count (re-seq #"/" cron-record))) "Only one interval is supported!")
    (doseq [x parts] (validate-cron-mapping x))
    (vec parts)))

(defn joda->cron [t]
  [(t/sec t) (t/minute t) (t/hour t) (t/day t) (t/month t) (t/day-of-week t) (t/year t)])

(defn cron->joda [c]
  (let [tc (reverse c) 
        tc (cons (first tc) (subvec (vec tc) 2 7))
        t (to-local-date-time (apply t/date-time tc))]
    (assert (= (t/day-of-week t) (c 5)) "Cron day of the week is not valid!")
    t))

(defn current-cron-time? []
  "Returns current local time in CRON format"
  (let [t (local-now)]
    [(joda->cron t) t]))

(defn- valid-element? [element {:keys [fixed range sequence] :as mapping}]
  (let [fixed? (when fixed 
                 (if (= element fixed) :fixed))
        in-range? (when range 
                    (if (and (>= element (first range)) (<= element (second range))) :range))
        belongs? (when sequence 
                   (if (get sequence element) :sequence))
        valid? (if (every? nil? [fixed range sequence]) :any)]
    (or fixed? belongs? in-range? valid? nil)))

(defn- evaluate-mapping 
  "Evaluates current cron based on mapping. Return
  value is map.

  If element is valid, than returned values are:

  :range
  :sequence
  :fixed

  Else return value contains nil for each mapping
  that is not valid."
  [c mapping]
  (vec (for [x (range 0 (count c))] (valid-element? (c x) (mapping x)))))


(defn- valid-whish? [whishes mapping]
  "Retruns keyword if whish is valid cron
  mapping"
  (not-any? nil? (evaluate-mapping whishes mapping)))

(defn- next-years [y mapping]
  (cond
    (:range mapping) (range (-> mapping :range first) (-> mapping :range second inc))
    (:sequence mapping) (:sequence mapping)
    (:fixed mapping) [(:fixed mapping)]
    :else (range y (-> mapping :max inc))))

(defn- next-months [mapping]
  (cond
    (:range mapping) (range (-> mapping :range first) (-> mapping :range second inc))
    (:sequence mapping) (:sequence mapping)
    (:fixed mapping) [(:fixed mapping)]
    :else (range 1 13)))


(defn- next-days [y m mapping]
  (let [max-days (t/number-of-days-in-the-month (t/date-time y m))]
    (cond
      (:range mapping) (range (-> mapping :range first) (inc (min  max-days (-> mapping :range second))))
      (:sequence mapping) (remove #(> % max-days) (:sequence mapping))
      (:fixed mapping) (if (> (:fixed mapping) max-days) [] [(:fixed mapping)])
      :else (range 1  (inc (t/number-of-days-in-the-month (t/date-time y m)))))))

(defn next-date-time [mapping]
  (cond
    (:range mapping) (range (-> mapping :range first) (-> mapping :range second inc))
    (:sequence mapping) (:sequence mapping)
    (:fixed mapping) [(:fixed mapping)]
    :else (range (:min mapping) (-> mapping :max inc))))

(defn- next-cron-timestamp
  "Return next valid timestamp after input 
  timestamp"
  [timestamp mapping]
  (let [mapping (vec (reverse mapping))
        current-cron (vec (reverse (joda->cron timestamp)))
        day-mapping (replace mapping [0 2 3])
        day-of-the-week-mapping (nth mapping 1)
        day-time-mapping (replace mapping [0 2 3 4 5 6])
        found-dates (for [y (next-years (current-cron 0) (day-mapping 0)) 
                          m (next-months (day-mapping 1))
                          d (next-days y m (day-mapping 2)) :when (and  (valid-whish? [y m d] day-mapping) (valid-element? (t/day-of-week (t/date-time y m d)) day-of-the-week-mapping))
                          h (next-date-time (day-time-mapping 3)) :when (valid-whish? [y m d h] (subvec day-time-mapping 0 4))
                          minutes (next-date-time (day-time-mapping 4)) :when (valid-whish? [y m d h minutes] (subvec day-time-mapping 0 5))
                          s (next-date-time (day-time-mapping 5)) :when (and (t/after? (to-local-date-time (t/date-time y m d h minutes s)) timestamp) (valid-whish? [y m d h minutes s] day-time-mapping))]
                      ;;(do (println y m d h minutes s)
                      (list y m d h minutes s))
        found-date (first found-dates)]
    (when found-date (to-local-date-time (apply t/date-time found-date)))))

(defn next-interval-timestamp [timestamp mapping]
  (when (some :interval mapping)
    (let [d (map #(if (:interval %) (:interval %) 0) mapping)
          delta (interleave [t/secs t/minutes t/hours t/days t/months t/weeks t/years] d)]
      (reduce t/plus timestamp (map eval (partition 2 delta))))))

(defn test-string "12 12 13 * * 7 2013")

(defn- test-find-valid-day? [s]
  (let [[c t] (current-cron-time?)
        mapping (parse-cron-string s)]
    (println "Next interval timestamp: " (time (next-interval-timestamp t mapping)))
    (println "Next CRON timestamp: " (time (next-cron-timestamp t mapping)))
    (or (next-interval-timestamp t mapping) (next-cron-timestamp t mapping))))

(defn print-cron [cron-string]
  (let [elements (map clojure.string/trim (clojure.string/split cron-string #" "))
        mapping '("Seconds: " "Minutes: " "Hours: " "Day of the month: " "Month: " "Day of the week: " "Year: ")]
    (println (map #(str %) (interleave mapping elements)))))
