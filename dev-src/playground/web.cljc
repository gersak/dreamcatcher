(ns playground.web
  #?(:cljs (:require [goog.dom :as gdom]
                     [sablono.core :as s :refer-macros [html]]
                     [om.next :as om :refer-macros [defui]]
                     [om.dom :as dom])))

#?(:cljs (enable-console-print!))


(defrecord BPMState [state-name state-description])

(defn draw-circle 
  ([[x y :as position] radius]
   [:circle.stm-state
    {:cx (str x)
     :cy (str y)
     :r (str radius)}]))

(defn center-state-name 
  "Returns dx and dy for text centralization"
  [text font-size]
  (let [letters (count text)
        scale-factor-x 1.78333
        scale-factor-y 2.25
        dy (* font-size (/ scale-factor-y 2))
        dx (* font-size letters (/ scale-factor-x 2))]
    [dx dy]))

(defn draw-state [parent [x y :as position] state-name]
  (let [radius 30
        font-size 16
        dy (/ font-size 2.25)]
    [:g.stm-state 
     {:id (str "dreamcatcher/" state-name)
      :onMouseDown #(.mouse-down parent %)
      :onMouseMove #(.mouse-move parent %)
      :onMouseUp #(om/update-state! parent assoc :pressed? false :x 0 :y 0)}
     (draw-circle position radius)
     [:text.stm-state 
      {:x x 
       :y y 
       :font-size font-size
       :dy dy}
      (str state-name)]]))


(defui DreamcatcherState
  static om/Ident
  (ident [this props]
         [:state-name (:name props)])
  Object
  (initLocalState [this]
                  (select-keys (om/props this)
                               [:x :y :name]))
  (render [this]
          (let [radius 30
                font-size 16
                dy (/ font-size 2.25)
                {:keys [x y name]} (om/get-state this)]
            (html 
              [:g.stm-state 
               {:on-click #(om/update-state! this assoc :pressed?  (-> this om/get-state :pressed? not))
                :on-mouse-move (fn [e] 
                                 (when-let [pressed? (-> this om/get-state :pressed?)]
                                   (om/update-state! this assoc :x (.-pageX e) :y (.-pageY e))))}
               (draw-circle [x y] radius)
               [:text.stm-state 
                {:x x 
                 :y y 
                 :font-size font-size
                 :dy dy}
                (str name)]]))))


(def dreamcatcher-state (om/factory DreamcatcherState))

(defui DreamcatcherModeller
  Object
  (render [this]
          (html 
            [:div
             [:svg.bpm
              (dreamcatcher-state {:x 100 :y 100 :name "START"})
              (dreamcatcher-state {:x 200 :y 200 :name "END"})]])))

(def dreamcatcher-modeller (om/factory DreamcatcherModeller))

(js/ReactDOM.render (dreamcatcher-modeller) (gdom/getElement "app"))
