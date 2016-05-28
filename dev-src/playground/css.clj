(ns playground.css
  (:require 
    [garden.def :refer [defrule defstyles]]
    [garden.stylesheet :refer [rule]]
    [garden.core :refer [css]]))

(defn get-public-picture [picture-file-name]
  (str "url('../pictures/" picture-file-name "')"))


(defstyles login
  (let [body (rule :body)]
    [["body"
      {:font-family "Helvetica Neue"
       :font-size "100px"
       :line-height 2.5
       ;:background (get-public-picture "tile_test.jpg")
       ;:background-color "black"
       :color "#fffffff"}]]))

(defstyles dreamcatcher-bpm
  (let [line-color "#9ACCFF"
        fill-color "black"
        hover-line-color "#02FF00"
        hover-fill-color "#3D4AFF"
        line-width 3
        font-size 16]
    [["svg.bpm"
      {:height "1000px"
       :width "100%"}
      ["g.stm-state"
       ["circle.stm-state" {"stroke" line-color
                            :stroke-width line-width
                            :fill fill-color}]
       [:text.stm-state {:text-anchor "middle"
                         :fill line-color}]]
      ;; Hover
      [:g.stm-state:hover
       [:circle.stm-state {:stroke hover-line-color
                           :fill hover-fill-color}]
       [:text.stm-state {:fill hover-line-color}]]
      [:g.stm]]]))
