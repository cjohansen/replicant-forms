(ns toil.ui
  (:require [datascript.core :as d]
            [phosphor.icons :as icons]
            [replicant.hiccup :as hiccup]))

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

(defn get-tasks [db]
  (->> (d/q '[:find ?e ?created
              :where [?e :task/created-at ?created]]
            db)
       (sort-by second >)
       (mapv #(d/entity db (first %)))
       seq))

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

(def priorities
  [{:value :task.priority/high
    :label "High"}
   {:value :task.priority/medium
    :label "Medium"}
   {:value :task.priority/low
    :label "Low"}])

(defn render-edit-form [form task]
  [:form.my-4.flex.flex-col.gap-4
   {:on {:submit [[:event/prevent-default]
                  [:form/submit :forms/edit-task (:db/id task)]]}}
   (text-input task :db/id {:type "hidden" :data-type "number"})
   (input-field form "Task" task :task/name text-input)
   (input-field form "Duration" task :task/duration text-input {:type "number"})
   (input-field form "Priority" task :task/priority select priorities)
   (input-field form "Complete?" task :task/complete? checkbox)
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
        (render-edit-form (d/entity db [:form/id :forms/edit-task]) task)
        (render-task task))])])

(defn render-page [db]
  [:main.md:p-8.p-4.max-w-screen-m
   [:h1.text-2xl.mb-4 "Practice log"]
   (render-task-form db)
   (render-tasks db)])

