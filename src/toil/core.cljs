(ns toil.core
  (:require [clojure.walk :as walk]
            [datascript.core :as d]
            [replicant.dom :as r]
            [toil.forms :as forms]
            [toil.schema :as schema]
            [toil.task :as task]
            [toil.ui :as ui]))

(def forms
  (->> [task/edit-form]
       (map (juxt :form/type identity))
       (into {})))

(defn get-input-value [^js element]
  (cond
    (= "number" (.-type element))
    (when (not-empty (.-value element))
      (.-valueAsNumber element))

    (= "checkbox" (.-type element))
    (if (.hasAttribute element "value")
      (when (.-checked element)
        (.-value element))
      (.-checked element))

    (= "keyword" (aget (.-dataset element) "type"))
    (keyword (.-value element))

    (= "number" (aget (.-dataset element) "type"))
    (when (not-empty (.-value element))
      (parse-long (.-value element)))

    (= "boolean" (aget (.-dataset element) "type"))
    (= "true" (.-value element))

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

(defn interpolate [event actions & [interpolations]]
  (walk/postwalk
   (fn [x]
     (or (get interpolations x)
         (case x
           :event/target.value (.. event -target -value)
           :event/form-data (some-> event .-target gather-form-data)
           :clock/now (js/Date.)
           x)))
   actions))

(declare execute-actions)

(defn submit-form [conn ^js event form-type id & args]
  (let [form-data (gather-form-data (.-target event))
        actions (apply forms/submit
                       (assoc (get forms form-type) :form/id [form-type id])
                       form-data
                       id
                       args)]
    (->> (interpolate event actions {:event/form-data form-data})
         (execute-actions conn event))))

(defn validate-form [conn ^js event form-id]
  (->> (forms/validate
        (assoc (get forms (first form-id)) :form/id form-id)
        (gather-form-data (.closest (.-target event) "form")))
       (execute-actions conn event)))

(defn transact-w-nils [conn txes]
  (d/transact!
   conn
   (mapcat
    (fn [tx]
      (if (map? tx)
        (let [nil-ks (map key (filter (comp nil? val) tx))
              identity (or (:db/id tx)
                           (when-let [attr (some tx schema/unique-attrs)]
                             [attr (attr tx)]))]
          (conj (for [k nil-ks]
                  [:db/retract identity k])
                (apply dissoc tx nil-ks)))
        [tx]))
    txes)))

(defn execute-actions [conn ^js event actions]
  (doseq [[action & args] (remove nil? actions)]
    (apply prn action args)
    (case action
      :event/prevent-default (.preventDefault event)
      :form/validate (apply validate-form conn event args)
      :form/submit (apply submit-form conn event args)
      :db/transact (apply d/transact! conn args)
      :db/transact-w-nils (apply transact-w-nils conn args)
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
