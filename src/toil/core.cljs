(ns toil.core
  (:require [clojure.walk :as walk]
            [datascript.core :as d]
            [replicant.dom :as r]
            [toil.forms :as forms]
            [toil.task :as task]
            [toil.ui :as ui]))

(defn get-input-value [^js element]
  (cond
    (= "number" (.-type element))
    (when (not-empty (.-value element))
      (.-valueAsNumber element))

    (= "number" (aget (.-dataset element) "type"))
    (when (not-empty (.-value element))
      (parse-long (.-value element)))

    (= "checkbox" (.-type element))
    (if (.hasAttribute element "value")
      (when (.-checked element)
        (.-value element))
      (.-checked element))

    (= "keyword" (aget (.-dataset element) "type"))
    (keyword (.-value element))

    :else
    (.-value element)))

(defn get-input-key [^js element]
  (when-let [k (some-> element .-name not-empty keyword)]
    (when (or (not= "checkbox" (.-type element))
              (.-checked element)
              (not (.hasAttribute element "value")))
      k)))

(defn gather-form-input-data [form-inputs]
  (some-> (into-array form-inputs)
          (.reduce
           (fn [res ^js el]
             (let [k (get-input-key el)]
               (cond-> res
                 k (assoc k (get-input-value el)))))
           {})))

(defn gather-form-data [^js form-el]
  (gather-form-input-data (.-elements form-el)))

(defn interpolate [event actions]
  (walk/postwalk
   (fn [x]
     (case x
       :event/target.value (.. event -target -value)
       :event/form-data (some-> event .-target gather-form-data)
       :clock/now (js/Date.)
       x))
   actions))

(def forms
  (->> [task/edit-form]
       (map (juxt :form/id identity))
       (into {})))

(declare execute-actions)

(defn submit-form [conn ^js event form-id & args]
  (->> (apply forms/submit
              (get forms form-id)
              (gather-form-data (.-target event))
              args)
       (execute-actions conn event)))

(defn validate-form [conn ^js event form-id]
  (->> (forms/validate
        (get forms form-id)
        (gather-form-data (.closest (.-target event) "form")))
       (execute-actions conn event)))

(defn execute-actions [conn ^js event actions]
  (doseq [[action & args] (remove nil? actions)]
    (apply prn action args)
    (case action
      :event/prevent-default (.preventDefault event)
      :form/validate (apply validate-form conn event args)
      :form/submit (apply submit-form conn event args)
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
