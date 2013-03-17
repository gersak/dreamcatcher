;*CLJSBUILD-MACRO-FILE*; 

(ns dreamcatcher.macros
  (:use dreamcatcher.util))

(defmacro add-statemachine-mapping [fun-name mapping]
  "Creates two funcitons with prefixes add- and remove-
  attached to \"fun-name\""
  (let [adder (symbol (str "add-" fun-name))
        remover (symbol (str "remove-" fun-name))]
    `(do
       (defn ~adder [stm# from-state# to-state# fun#]
         (let [from-states# (if-not (coll? from-state#) (list from-state#) from-state#)
                to-states# (if-not (coll? to-state#) (list to-state#) to-state#)]
            (when (fn? fun#)
              (map #(assert (contains? @stm# %) (str "There is no " % " in " stm#)) from-states#)
              (map #(assert (contains? @stm# %) (str "There is no " %" in " stm#)) to-states#)
              (doseq [x# from-states# y# to-states#]
                (swap! stm# #(assoc % x# (assoc (get-state-mapping @stm# x#)
                                               ~mapping (merge (get-transitions @stm# x#) (hash-map y# fun#)))))))))
       (defn ~remover [stm# from-state# to-state#]
         (do 
           (assert (and (contains? @stm# from-state#) (contains? @stm# to-state#)) "STM doesn't contain all states")
           (swap! stm# #(merge % 
                               (hash-map from-state# 
                                         (hash-map :transitions (dissoc (get-transitions @stm# from-state#) to-state#))))))))))
