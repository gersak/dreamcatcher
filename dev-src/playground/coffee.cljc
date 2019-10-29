(ns playground.coffee
  #?(:cljs 
     (:require-macros [clojure.core.async :refer [go]]))
  (:require [dreamcatcher.async :refer :all :as da]
            [dreamcatcher.core :as d 
             #?@(:clj [:refer [defstm]]
                 :cljs [:refer-macros [defstm]])]
            #?(:clj [dreamcatcher.viz :as viz])
            [clojure.core.async :as async 
             :refer [mult mix chan tap 
                     admix close! put! take! #?(:clj go)]]))


(defn wait [timeout]
  (async/<!! (async/timeout timeout)))


(defn is-beverage [beverage]
  {:pre [(set? beverage)
         (not-empty beverage)]}
  (fn [instance] 
    (let [{beverage' :beverage/type} (d/data instance)]
      (beverage beverage'))))

(defn beverage-maker-action [instance message timeout]
  (println message)
  (wait timeout)
  instance)

(defstm BEVERAGE_MAKER
  ;; Transitions
  [:make-beverage :black 
   (fn [instance]
     (-> 
       instance
       (beverage-maker-action "Heating coffee" 2000)
       (beverage-maker-action "Drip, drop..." 1000)))

   :make-beverage :milk identity

   d/any-state :milk
   (fn [instance]
     (-> instance
       (beverage-maker-action "Heating milk!" 500)
       (beverage-maker-action "Shhhhhhhhh" 300)))

   d/any-state :chocolate
   (fn [instance]
     (-> instance
       (beverage-maker-action "Adding chocolate!" 100)
       (beverage-maker-action "crop, crop..." 200)))

   :make-beverage :espresso
   (fn [instance]
     (-> 
       instance
       (beverage-maker-action "Pouring espresso" 2000)
       (beverage-maker-action "Drip, drop..." 1000)))

   [:espresso :black] :milk identity

   :milk :chocolate identity

   [:black :milk :chocolate :espresso] :sugar
   (fn [instance]
     (let [{sugar :beverage/sugar-weight} (d/data instance)]
       (beverage-maker-action instance "Adding sugar" 500)))

   :sugar :beverage-made
   (fn [instance]
     (let [{beverage :beverage/type :as data} (d/data instance)]
       (println (format "%s finished!" beverage))
       (d/set-data! instance (dissoc data :beverage/type))))]

  ;; Validators
  [:make-beverage :black         (is-beverage #{"Black Coffee"})
   :make-beverage :espresso      (is-beverage #{"Espresso" "Espressino"})
   :make-beverage :milk          (is-beverage #{"Cacao" "Latte" "Milk"})
   :milk          :chocolate     (is-beverage #{"Cacao"})
   :milk          :sugar         (complement (is-beverage #{"Cacao"}))
   :black         :milk          (is-beverage #{"Latte"})
   :black         :sugar         (is-beverage #{"Black Coffee"})
   :espresso      :milk          (is-beverage #{"Espressino"})
   :espresso      :sugar         (is-beverage #{"Espresso"})])

(defn add-money [instance amount]
  (d/update-data! instance update :money/balance (fnil + 0) amount))

(defn money-status [instance]
  (get (d/data instance) :money/balance 0))

(defn return-money [instance] 
  (d/update-data! instance assoc :money/balance 0))

(defn print-message [instance message]
  (println message)
  instance)

(defn accept-money []
  (print "Insert money: ")
  (flush)
  (loop [cnt 0
         money (read-line)]
    (let [money' (read-string money)] 
      (if (>= cnt 5)
        (do
          (println "Die of thirst you RETARD!")
          0)
        (if (and (number? money') 
                 (not (neg? money')))
          money'
          (do
            (println "Type in positive or 0!")
            (flush)
            (recur (inc cnt) (read-line))))))))

(defstm PIGGIE_BOX
  [d/any-state :insert-money
   (fn [instance]
     (let [money (accept-money)
           instance' (add-money instance money)]
       ; (print-message instance (str "Balance $" (money-status instance')))
       instance'))
   d/any-state :return-money
   (fn [instance]
     (if (pos? (money-status instance))
       (do
         (print-message instance (str "Returning $" (money-status instance)))
         (return-money instance))
       instance))])

(def beverages 
  (array-map 
    1 {:name "Black Coffee" :price 2}
    2 {:name "Latte" :price 2.5}
    3 {:name "Espresso" :price 2}
    4 {:name "Espressino" :price 2}
    5 {:name "Cacao" :price 1.5}
    6 {:name "Milk" :price 1}))


(defn parse [choice options]
  (try
    (let [choice' (Integer/parseInt choice)]
      (when (and 
              (pos? choice') 
              (<= choice' (count options)))
        choice'))
    (catch Throwable _ nil)))

(defn choose-bevarage [instance]
  (println "Beverages:")
  (println (clojure.string/join "\r\n" (map (fn [[k {v :name}]] (str k ". " v)) beverages)))
  (letfn [(prompt []
            (print "Type in beverage # ")
            (flush)
            (read-line))] 
    (loop [choice (prompt)]
      (if-let [choice (parse choice beverages)]
        (->
          instance
          (d/update-data! assoc :beverage/type (get-in beverages [choice :name]))
          (d/update-data! assoc :beverage/price (get-in beverages [choice :price])))
        (recur (prompt))))))

(defn choose [instance]
  (let [choices (array-map 1 "Insert Money"
                           2 "Choose Beverage"
                           3 "Return Money"
                           4 "Shutdown")]
    (println "Choose one of:")
    (println (clojure.string/join "\r\n" (map (fn [[k v]] (str k ". " v)) choices)))
    (letfn [(prompt []
              (print "Type in your choice # ")
              (flush)
              (read-line))] 
      (loop [choice (prompt)]
        (if-let [choice (parse choice choices)]
          (d/update-data! instance assoc :vending-machine/selected (get choices choice))
          (recur (prompt)))))))

(defn is-selected [choice]
  (fn [instance] 
    (let [{choice' :vending-machine/selected} (d/data instance)]
      (= choice choice'))))

(defn enough-money? [instance]
  (let [{price :beverage/price
         status :money/balance
         :or {status 0}} (d/data instance)]
    (>= status price)))

(def not-enough-money? (complement enough-money?))

(defstm VENDING_MACHINE
  ;; Transitions
  [d/any-state :select-beverage choose-bevarage

   :start :choose choose

   :choose [:insert-money :select-beverage :return-money :shutdown] identity
   [:insert-money :select-beverage :return-money :shutdown] d/any-state
   (fn [instance]
     (let [data (d/data instance)]
       (d/set-data! instance (dissoc data :vending-machine/selected))))

   :select-beverage :end
   (fn [instance] 
     (let [{price :beverage/price
            status :money/balance
            :or {status 0}} (d/data instance)]
       (println (str "Not enough money. Price $" price 
                     " and current status is $" status)))
     (d/update-data! instance dissoc :beverage/type :beverage/price))

   :select-beverage :make-beverage identity

   d/any-state :beverage-made
   (fn [instance] 
     (let [{price :beverage/price
            status :money/balance
            :or {price 0
                 status 0}
            :as data} (d/data instance)] 
       (d/set-data! instance 
                    (->
                      data
                      (dissoc :beverage/price)
                      (assoc :money/balance (- status price))))))

   :beverage-made :return-money identity
   [:insert-money :return-money] :end identity

   d/any-state d/any-state
   (fn [before after]
     (let [[bs as] (map d/state [before after])
           {beverage :beverage/type
            status :money/balance} (d/data after)
           messages (cond-> [(str "Reached state: " as ".")]
                      (and (some? status) (pos? status)) (conj (str "Balance: $" status "."))
                      (and (some? status) (zero? status)) (conj (str "Balance is empty!"))
                      (some? beverage) (conj (str "Selected beverage: " beverage ".")))]
       (println (clojure.string/join " " messages))))]

  ;; Validators
  [d/any-state :insert-money (is-selected "Insert Money")
   d/any-state :select-beverage (is-selected "Choose Beverage")
   :choose :return-money (is-selected "Return Money")
   d/any-state :shutdown (is-selected "Shutdown")
   :select-beverage :make-beverage enough-money?
   :select-beverage :end not-enough-money?])

(d/composed-stm MACHINE VENDING_MACHINE PIGGIE_BOX BEVERAGE_MAKER)

(def machine-instance (d/make-machine-instance MACHINE :start))

(defn run-machine []
  (loop [instance machine-instance]
    (when-let [instance' (try
                           (d/reach-state instance :end)
                           (catch Throwable _
                             (println "Shuting down machine...")
                             nil))]
      ; (println "Moving from :end->:start")
      (recur (d/set-state! instance' :start)))))


(defn test-step [instance] 
  (println "Candidates: " (d/candidates? instance))
  (d/move instance (first (d/candidates? instance))))

(comment 
  (d/to-> machine-instance :end)
  (viz/draw-stm BEVERAGE_MAKER)
  (viz/draw-stm VENDING_MACHINE)
  (viz/draw-stm MACHINE))
