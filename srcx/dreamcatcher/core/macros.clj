(ns dreamcatcher.core.macros
  (:use [dreamcatcher.core.macros :only (make-state-machine)]))


(defmacro defstm
  "Macro defines stm with make-stat-machine function.
  STM is persistent hash-map."
  ([stm-name transitions] `(defn ~stm-name (make-state-machine ~transitions)))
  ([stm-name transitions validators] `(defn ~stm-name (make-state-machine ~transitions ~validators))))
