(ns toil.forms)

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

(defn submit-edit-task [data task-id]
  (if-let [errors (seq (validate-edit-task data))]
    [[:db/transact
      [{:form/id :forms/edit-task
        :form/validation-errors errors}]]]
    (let [nil-ks (map key (filter (comp nil? val) data))]
      [[:db/transact
        (into
         [(-> (apply dissoc data nil-ks)
              (assoc :db/id task-id)
              (assoc :task/editing? false))
          [:db/retractEntity [:form/id :forms/edit-task]]]
         (for [k nil-ks]
           [:db/retract task-id k]))]])))
