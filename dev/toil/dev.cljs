(ns toil.dev
  (:require [datascript.core :as ds]
            [toil.core :as app]
            [toil.schema :as schema]))

(defonce conn (ds/create-conn schema/schema))
(defonce el (js/document.getElementById "app"))

(defn ^:dev/after-load main []
  ;; Add additional dev-time tooling here
  (app/main conn el))
