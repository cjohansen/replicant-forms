(ns toil.forms
  (:require [replicant.hiccup :as hiccup]))

(defn keyword->s [k]
  (if-let [ns (namespace k)]
    (str ns "/" (name k))
    (name k)))

(defn text-input [m k & [attrs]]
  (let [id (keyword->s k)]
    [:input.grow.input.input-bordered
     (into
      {:type "text"
       :name id
       :id id
       :default-value (get m k)}
      attrs)]))

(defn select [m k options]
  (let [selected (get m k)
        id (keyword->s k)
        sample-value (-> options first :value)]
    [:select.grow.select.select-bordered
     (cond-> {:name id
              :id id}
       (keyword? sample-value)
       (assoc :data-type "keyword"))
     (for [{:keys [value label]} options]
       [:option
        (cond-> {:value (cond-> value
                          (keyword? value) keyword->s)}
          (= value selected) (assoc :default-selected true))
        label])]))

(defn checkbox [m k]
  [:input.checkbox
   (cond-> {:type "checkbox"
            :name (keyword->s k)}
     (get m k) (assoc :default-checked "checked"))])

(defn input-field [form label m k f & args]
  (let [error (->> (:form/validation-errors form)
                   (filter (comp #{k} :validation-error/field))
                   first)]
    (list [:div.flex.items-center
           [:label.basis-24 {:for (keyword->s k)} label]
           (cond-> (apply f m k args)
             error
             (hiccup/update-attrs
              #(-> %
                   (update :class conj "input-error")
                   (assoc-in
                    [:on :input]
                    [[:form/validate (:form/id form) k]]))))]
          (when error
            [:div.validator-hint.text-error.ml-24.-m-2.mb-2
             (:validation-error/message error)]))))

(defn validate-edit-task [data]
  (->> [(when (empty? (:task/name data))
          {:validation-error/field :task/name
           :validation-error/message "Please type in some text"})
        (when (< 60 (or (:task/duration data) 0))
          {:validation-error/field :task/duration
           :validation-error/message "Duration can not exceed 60 minutes"})]
       (remove nil?)))

(defn validate-edit-task-form [form-id data]
  [[:db/transact
    [{:form/id form-id
      :form/validation-errors (validate-edit-task data)}]]])

(defn submit-edit-task [form-type task-id data]
  (if-let [errors (seq (validate-edit-task data))]
    [[:db/transact
      [{:form/id [form-type task-id]
        :form/validation-errors errors}]]]
    (let [nil-ks (map key (filter (comp nil? val) data))]
      [[:db/transact
        (into
         [(-> (apply dissoc data nil-ks)
              (assoc :db/id task-id)
              (assoc :task/editing? false))
          [:db/retractEntity [:form/id [form-type task-id]]]]
         (for [k nil-ks]
           [:db/retract task-id k]))]])))
