(ns dreamcatcher.macros
  (:use dreamcatcher.util))

(defmacro add-statemachine-mapping [fun-name mapping]
  "Creates two funcitons with prefixes add- and remove-
  attached to \"fun-name\""
  (let [adder (symbol (str "add-" fun-name))
        remover (symbol (str "remove-" fun-name))]
    `(do
       (defn ~adder [stm# from-state# to-state# fun#]
         (when (fn? fun#)
           (assert (contains? @stm# from-state#) (str "There is no " from-state# " in " stm#))
           (assert (contains? @stm# to-state#) (str "There is no " to-state# " in " stm#))
           (swap! stm# #(assoc % from-state# (assoc (get-state-mapping @stm# from-state#)
                                                    ~mapping (merge (get-transitions @stm# from-state#) (hash-map to-state# fun#)))))))
       (defn ~remover [stm# from-state# to-state#]
         (do 
           (assert (and (contains? @stm# from-state#) (contains? @stm# to-state#)) "STM doesn't contain all states")
           (swap! stm# #(merge % 
                               (hash-map from-state# 
                                         (hash-map :transitions (dissoc (get-transitions @stm# from-state#) to-state#))))))))))
