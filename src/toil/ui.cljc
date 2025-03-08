(ns toil.ui
  (:require [datascript.core :as d]
            [phosphor.icons :as icons]))

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
     (:task/name task)]]])

(defn render-tasks [db]
  [:ol.mb-4.max-w-screen-sm
   (for [task (get-tasks db)]
     [:li.bg-base-200.my-2.px-4.py-3.rounded.w-full
      (render-task task)])])

(defn render-page [db]
  [:main.md:p-8.p-4.max-w-screen-m
   [:h1.text-2xl.mb-4 "Practice log"]
   (render-task-form db)
   (render-tasks db)])
