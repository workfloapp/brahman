(ns workflo.brahman.bridges.om-datascript
  (:require [datascript.core :as d]
            [workflo.brahman.model :as bm]))

(defn query-derived-attr
  [conn env attr query entity]
  (d/q (:query attr) @conn (:db/id entity) query))

(defn- identifying-model-attr
  [model]
  (first (filter
          (fn [attr]
            (let [attr-name (cond-> attr
                              (map? attr) ffirst)]
              (= (name (bm/model-name model))
                 (namespace attr-name))))
          (bm/attrs model))))

(defn datascript-query
  "Translate an Om Next query to a DataScript query based
   on the current DataScript schema."
  [{:keys [model fetch-one?]} query _]
  (let [model-attr (identifying-model-attr model)]
    (if fetch-one?
      ;; Query a single entity
      `[:find (~'pull ?e ~query) .
        :in $ ~'?id
        :where [~'?e ~model-attr]
        [~'?e :db/id ~'?id]]
      ;; Query a collection of entities
      `[:find [(~'pull ~'?e ~query)]
        :where [~'?e ~model-attr]])))

(defn query-store
  "Execute an Om Next query against DataScript."
  [conn env query params]
  (let [ds-query  (datascript-query env query params)
        ds-result (d/q ds-query @conn)]
    ds-result))
