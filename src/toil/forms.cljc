(ns toil.forms
  (:require [replicant.hiccup :as hiccup]))

(defn validate-field [field validation data]
  (case (:validation/kind validation)
    :required
    (when (empty? data)
      {:validation-error/field field
       :validation-error/message
       (or (:validation/message validation)
           "Please type in some text")})

    :max-num
    (when (< (:max validation) (or data 0))
      {:validation-error/field field
       :validation-error/message
       (or (:validation/message validation)
           (str "Should be max " (:max validation)))})))

(defn validate-form-data [form data]
  (->> (:form/fields form)
       (mapcat
        (fn [{:keys [k validations]}]
          (let [field-data (get data k)]
            (keep #(validate-field k % field-data) validations))))))

(defn validate [form data]
  [[:db/transact
    [{:form/id (:form/id form)
      :form/validation-errors (validate-form-data form data)}]]])

(defn submit [form data & args]
  (if-let [errors (seq (validate-form-data form data))]
    [[:db/transact
      [{:form/id (:form/id form)
        :form/validation-errors errors}]]]
    (let [actions (vec (or (:form/submit-actions form)
                           (when-let [handler (:form/handler form)]
                             (apply (handler form) data args))))
          idx (.indexOf (map first actions) :db/transact)
          idx (if (<= 0 idx)
                idx
                (.indexOf (map first actions) :db/transact-w-nils))
          cleanup-tx [:db/retractEntity [:form/id (:form/id form)]]]
      (if (<= 0 idx)
        (update-in actions [idx 1] conj cleanup-tx)
        (conj actions [:db/transact [cleanup-tx]])))))

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
       (keyword? sample-value) (assoc :data-type "keyword"))
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
