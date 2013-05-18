(ns playground.html
  (:use hiccup.core
        [hiccup.page :only (html5 include-css include-js)]
        [hiccup.element :only (javascript-tag)]))

(def web-page (html5 
                [:head
                 ;;(include-js "jQuery.js")
                 (include-js "Raphael.js")
                 (include-js "playground.js")
                 [:title "Playground server"]]
                [:body
                 [:h1 "Hello from Dreamcatcher"]
                 [:div {:id "STM" :width "100%" :height "100%"}]
                 (javascript-tag "playground.core.init();")]))

(defn generate-template []
  (do
    (println web-page)
    (spit "js/playground.html" web-page)))

(generate-template)
