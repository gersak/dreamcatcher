(ns playground-server.server
  (:require [cljs.nodejs :as node]))

(def http
  (node/require "http"))

(def fs (node/require "fs"))
(def path (node/require "path"))

(def homepage (.readFileSync fs "./playground.html"))

(defn serve-files [req resp]
  (let [url (.-url req)
        file-path (str "." url)]
    (if-let [stats (when (.existsSync fs file-path) (.lstatSync fs file-path))]
      (if (.isFile stats)
        (do
          ;;(.log js/console (str "Serving file: " url))
          (.writeHead resp 200 (clj->js {"Content-Type" (case (.extname path file-path)
                                                          ".js" "text/javascript"
                                                          ".css" "text/css")}))
          (.write resp (.readFileSync fs file-path)))
        (.writeHead resp 500))
      (do
        (.log js/console (str "There is no file: " url))
        (.writeHead resp 500)))))

(defn handler [req resp]
  (let [url (str (.-url req))]
    ;;(.log js/console (str "URL: " url))
    (case url
      "/" (do 
            (.writeHead resp 200 (clj->js {"Content-Type" "text/html"}))
            (.end resp homepage))
      (serve-files req resp))
    (.end resp)))
      ;;(.end resp (serve-files req resp)))))

(defn start [& _]
  (let [server (.createServer http handler)]
    (.listen server 1337 "127.0.0.1")
    (println "Server running at http://localhost:1337")))

(set! *main-cli-fn* start)
