(ns dreamcatcher.macros)

(def ^:dynamic *warn-on-duplicate* true)

(defmacro add-statemachine-mapping [fun-name mapping]
      "Creates two funcitons with prefixes add- and remove-
      attached to \"fun-name\""
      (let [adder (symbol (str "add-" fun-name))
            remover (symbol (str "remove-" fun-name))]
            `(do
(defn ~adder
      "Function accepts stm as clojure.lang.Atom and
      adds mapping to that stm from state to another
      state with input function"
      [stm# from-state# to-state# fun#]
      (let [from-states# (cond
                           (fn? from-state#) (from-state#)
                           (false? (coll? from-state#)) (list from-state#)
                           :else from-state#)
            to-states# (cond
                         (fn? to-state#) (to-state#)
                         (false? (coll? to-state#)) (list to-state#)
                         :else to-state#)]
            (map #(assert (contains? @stm# %) (str "There is no " % " in " stm#)) from-states#)
            (map #(assert (contains? @stm# %) (str "There is no " %" in " stm#)) to-states#)
            (doseq [x# from-states# y# to-states#]
                  (let [cm# (get-in @stm# [x# ~mapping])
                        ; nm# (reduce conj (array-map) (conj cm# [y# fun#]))
                        nm# (assoc cm# y# fun#)
                        mm# (meta @stm#)]
                        ; (clojure.core/println "HI")
                        (when (and
                                (some? (get cm# y#))
                                (not= fun# (get cm# y#)))
                         #?(:clj (println (str "WARN: Overwriting " (get mm# :dreamcatcher/stm) " " ~(str fun-name) " " (pr-str x#) " -> " (pr-str y#)))
                            :cljs (.log js/console (str "WARN: Overwriting " (get mm# :dreamcatcher/stm) " " ~(str fun-name) " " (pr-str x#) " -> " (pr-str y#)))))
                        (reset! stm# 
                               (with-meta 
                                     (assoc-in @stm# [x# ~mapping] nm#)
                                     (update mm# ~mapping conj from-state# to-state# fun#)))))))

(defn ~remover
      "Removes mapping function from state to
      another state in given STM."
      [stm# from-state# to-state#]
      (assert (and (contains? @stm# from-state#) (contains? @stm# to-state#)) "STM doesn't contain all states")
      (let [change-data# (dissoc (-> @stm# from-state# ~mapping) to-state#)
            update# (assoc @stm# from-state# (merge (-> @stm# from-state#) (array-map ~mapping change-data#)))]
            (reset! stm# update#))))))
