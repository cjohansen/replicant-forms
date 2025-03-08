(ns toil.core
  (:require [clojure.walk :as walk]
            [datascript.core :as d]
            [replicant.dom :as r]
            [toil.ui :as ui]))

(defn interpolate [event actions & [interpolations]]
  (walk/postwalk
   (fn [x]
     (case x
       :event/target.value (.. event -target -value)
       x))
   actions))

(defn execute-actions [conn ^js event actions]
  (doseq [[action & args] (remove nil? actions)]
    (apply prn action args)
    (case action
      :db/transact (apply d/transact! conn args)
      (println "Unknown action" action "with arguments" args))))

(defn main [conn el]
  (add-watch
   conn ::render
   (fn [_ _ _ _]
     (r/render el (ui/render-page (d/db conn)))))

  (r/set-dispatch!
   (fn [{:keys [replicant/dom-event]} actions]
     (->> actions
          (interpolate dom-event)
          (execute-actions conn dom-event))))

  ;; Trigger the initial render
  (d/transact! conn [{:db/ident :system/app
                      :app/started-at (js/Date.)}]))
