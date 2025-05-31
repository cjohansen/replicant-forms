(ns toil.ui
  (:require [toil.task :as task]))

(defn render-page [db]
  [:main.md:p-8.p-4.max-w-screen-m
   [:h1.text-2xl.mb-4 "Practice log"]
   (task/render-task-form db)
   (task/render-tasks db)])
