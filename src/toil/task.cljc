(ns toil.task
  (:require [datascript.core :as d]
            [phosphor.icons :as icons]
            [toil.forms :as forms]))

(defn edit-task [data task-id]
  (let [nil-ks (map key (filter (comp nil? val) data))]
    [[:db/transact
      (into
       [(-> (apply dissoc data nil-ks)
            (assoc :db/id task-id)
            (assoc :task/editing? false))]
       (for [k nil-ks]
         [:db/retract task-id k]))]]))

(def edit-form
  {:form/type :forms/edit-task
   :form/fields
   [{:k :task/name
     :validations [{:validation/kind :required}]}
    {:k :task/duration
     :validations
     [{:validation/kind :max-num
       :validation/message "Duration can not exceed 60 minutes"
       :max 60}]}]

   :form/submit-actions
   [[:db/transact-w-nils [:event/form-data]]]})

(def priorities
  [{:value :task.priority/high
    :label "High"}
   {:value :task.priority/medium
    :label "Medium"}
   {:value :task.priority/low
    :label "Low"}])

(defn get-tasks [db]
  (->> (d/q '[:find ?e ?created
              :where [?e :task/created-at ?created]]
            db)
       (sort-by second >)
       (mapv #(d/entity db (first %)))
       seq))

(defn render-task-form [db]
  (let [text (:task/name (d/entity db [:db/ident ::task]))]
    [:form.mb-4.flex.gap-2.max-w-screen-sm
     {:on {:submit [[:event/prevent-default]
                    (when-not (empty? text)
                      [:db/transact
                       [{:task/name text
                         :task/created-at :clock/now}
                        {:db/ident ::task
                         :task/name ""}]])]}}
     [:input.input.input-bordered.w-full
      {:type "text"
       :name "name"
       :value text
       :placeholder "What do you need to practice?"
       :on {:input [[:db/transact
                     [{:db/ident ::task
                       :task/name :event/target.value}]]]}}]
     [:button.btn.btn-primary
      {:type "submit"
       :class (when (empty? text)
                "btn-disabled")}
      "Add"]]))

(defn render-edit-form [form task]
  [:form.my-4.flex.flex-col.gap-4
   {:on {:submit [[:event/prevent-default]
                  [:form/submit :forms/edit-task (:db/id task)]]}}
      (forms/text-input task :db/id {:type "hidden" :data-type "number"})
   (forms/text-input task :task/editing? {:type "hidden"
                                          :value "false"
                                          :data-type "boolean"})
   (forms/input-field form "Task" task :task/name forms/text-input)
   (forms/input-field form "Duration" task :task/duration forms/text-input {:type "number"})
   (forms/input-field form "Priority" task :task/priority forms/select priorities)
   (forms/input-field form "Complete?" task :task/complete? forms/checkbox)
   [:div.flex.flex-row.gap-4
    [:button.btn.btn-primary {:type "submit"}
     "Save"]]])

(defn render-task [task]
  [:div.flex.place-content-between
   [:button.cursor-pointer.flex.items-center
    {:aria-label (if (:task/complete? task)
                   "Click to complete"
                   "Click to un-complete")
     :on {:click [[:db/transact
                   [{:db/id (:db/id task)
                     :task/complete? (not (:task/complete? task))}]]]}}
    (if (:task/complete? task)
      [:span.w-8.pr-2.tilt.transition.duration-1000
       {:replicant/key :done
        :replicant/mounting {:class ["text-success"]}}
       (icons/render (icons/icon :phosphor.regular/check-square)
                     {:focusable "false"})]
      [:span.w-8.pr-2
       (icons/render (icons/icon :phosphor.regular/square)
                     {:focusable "false"})])
    [:span {:class (when (:task/complete? task)
                     "line-through")}
     (:task/name task)]]
   [:button.w-6
    {:aria-label "Edit"
     :on {:click [[:db/transact
                   [[:db/add (:db/id task)
                     :task/editing? true]]]]}}
    (icons/render
     (icons/icon :phosphor.regular/gear)
     {:focusable "false"})]])

(defn render-tasks [db]
  [:ol.mb-4.max-w-screen-sm
   (for [task (get-tasks db)]
     [:li.bg-base-200.my-2.px-4.py-3.rounded.w-full
      (if (:task/editing? task)
        (render-edit-form
         (d/entity db [:form/id [:forms/edit-task (:db/id task)]])
         task)
        (render-task task))])])
