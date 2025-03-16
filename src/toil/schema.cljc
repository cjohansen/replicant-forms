(ns toil.schema)

(def schema
  {:form/id {:db/unique :db.unique/identity}})

(def unique-attrs
  (->> schema
       (filterv (comp :db/unique val))
       (mapv first)
       set))
