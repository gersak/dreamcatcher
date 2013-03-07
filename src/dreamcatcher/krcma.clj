(ns dreamcatcher.krcma
  (:use dreamcatcher.core))

(declare probaj-naruciti)

(def cjenik {:sok 12
             :kava 9
             :pivo 10})

(def skladiste (atom {:pivo 5
                      :kava 10
                      :sok 7}))

(defn zove-konobara [machine] (println (str (-> machine get-data :ime) ": Ej ti tamo!")))

(defn odabir-cuge [novac] 
  (let [dostupna-pica (remove #(> (val %) novac) cjenik)
        izbor (when (seq dostupna-pica) (-> dostupna-pica rand-nth))]
    (when izbor (-> izbor first))))

(defn bira-cugu [machine] (let [novac (-> machine get-data :novac)
                                ime (-> machine get-data :ime)
                                izbor (-> machine get-data :izbor)]
                            (println (str (-> machine get-data :ime) ": Popiti cu " 
                                          (-> machine get-data :izbor)))
                            (println (str ime ": Placa " (get cjenik izbor) "kn."))
                            (assoc-data machine {:novac (- novac (get cjenik izbor))
                                            :naruceno? true})
                            (println (str ime ": Imam jos " novac " kn."))))

(defn pije [machine] (do 
                       (let [casa (apply max [0 (- (-> machine get-data :casa) (* 100 (rand)))])
                             ostatak (int casa) 
                             ime  (-> machine get-data :ime)]
                         (println (str ime  ": Glogloglog..."))
                         (println ime ": Ostalo mi je još " ostatak "% pica.")
                         (if (zero? ostatak)
                           (assoc-data machine {:popijene-cuge (-> machine get-data :popijene-cuge inc)
                                                :naruceno? false
                                                :casa ostatak})
                           (assoc-data machine {:casa casa})))))

(defn ide-doma [im]
  (println (-> im get-data :ime) "Idem doma..."))

(defn dosada [machine] (println "Fiiffiriffi..."))



;; GOST statemachine
(def gost-automat (make-state-machine [:cekam :zove zove-konobara 
                                       :zove :cekam (fn [im] (do
                                                               (println (-> im get-data :ime) ": Fiiffiriffi...")
                                                               (Thread/sleep 2000)))
                                       :cekam :narudzba (fn [_]
                                                          (println (rand-nth ["Pa gdje ste dosada..."
                                                                              "Dobar dan."
                                                                              "Jako ste brzi."]))) 
                                       :narudzba :cekam bira-cugu
                                       :cekam :doma ide-doma
                                       :doma :cekam (fn [_] (println "Woohooo... Evo me opet!"))
                                       :pije :cekam (fn [im] 
                                                      (do 
                                                        (println (-> im get-data :ime) " : Uf, ovo je godilo!")
                                                        (Thread/sleep (+ (rand-int 1000) 9000))))
                                       :cekam :cekam (fn [im] 
                                                       (do 
                                                         (println (-> im get-data :ime) ": Fiiffiriffi...")
                                                         (Thread/sleep (rand-nth [1000
                                                                                  3000
                                                                                  7000]))))
                                       :cekam :pije (fn [im]
                                                      (println (-> im get-data :ime) ": Napunila mi se casa!"))
                                       :pije :pije pije
                                       :doma :doma (fn [im] (println "Vec sam doma i imam samo " (-> im get-data :novac) " kune."))]))



(defn primi-narudzbu [_] (println "Slusam..."))
(defn provjeri-skladiste [_] (println "Provjeravam skladiste"))
(defn naplati-narudzbu [_] (println "Naplacujem"))
(defn zatvaram-kapiju [_] (println "Ajmo, fajrunt"))

(def konobar-automat (make-state-machine [:slobodan :narudzba primi-narudzbu
                                          :slobodan :slobodan dosada
                                          :narudzba :slobodan dosada 
                                          :narudzba :skladiste provjeri-skladiste
                                          :skladiste :narudzba primi-narudzbu
                                          :skladiste :slobodan naplati-narudzbu
                                          :skladiste :kraj zatvaram-kapiju]))

;; Gost
(defn get-gost [ime novac popijene-cuge]
  (get-machine-instance gost-automat :cekam {:novac novac
                                             :ime ime
                                             :popijene-cuge 0
                                             :konobar-stigao? false
                                             :naruceno? false
                                             :casa 0}))



(defn gost-pattern [im]
  (let [state (get-state im)
        data (get-data im)
        novac (:novac data)
        ime (:ime data)
        izbor (odabir-cuge novac)]
    (cond
      (= state :doma) (if izbor 
                        (move im :doma)
                        (move im :cekam))
      (= :cekam state) (cond
                         (nil? izbor) (move im :doma)
                         (and (:konobar-stigao? data) (not (:naruceno? data))) (move im :narudzba)
                         (and (:naruceno? data) (zero? (:casa data))) (move im :cekam)
                         (and (not (:konobar-stigao? data)) (not (:naruceno? data))) (move im :zove)
                         (not= 0 (:casa data)) (move im :pije)
                         :else (move im :cekam))
      (= :zove state) (move im :cekam)
      (= :pije state) (if (zero? (:casa data)) (move im :cekam) (move im :pije))
      (= :narudzba state) (do
                            (assoc-data im {:izbor izbor})
                            (move im :cekam)))))

(def ^:dynamic *bar-opened* true)



(defn gost-act [im]
  (when *bar-opened*
    (Thread/sleep 1000)
    (send-off *agent* #'gost-act)
    (gost-pattern im)))

(def mirko (agent (get-gost "Mirko" 1000 0)))
