(ns brahman.backends.datascript
  (:require [brahman.model :as bm]))

;;;; Schema attribute extraction

(defn- attr-name [schema-name short-name]
  (keyword (name schema-name) (name short-name)))

(defn- join? [[short-name spec]]
  (and (map? spec) (= 1 (count spec))))

(defn- gather-attr-names [model-name schema]
  {:pre [(map? schema)]}
  (into []
        (comp (remove join?)
              (map (fn [[name attr-spec]]
                     (attr-name model-name name))))
        schema))

(declare schema->attrs)

(defn- gather-joins [model schema]
  {:pre [(map? schema)]}
  (letfn [(recur-join [target-name]
            (let [modeler (bm/get-modeler model)
                  target  (bm/get-model modeler target-name)]
              (schema->attrs target)))
          (join-attr [schema-name short-name spec]
            (let [[spec target] (first spec)]
              (hash-map (attr-name schema-name short-name)
                        (cond-> target
                          (not= target '...) recur-join))))
          (gather-join [res [short-name spec]]
            (cond-> res
              (map? spec) (conj (join-attr (bm/model-name model)
                                           short-name
                                           spec))))]
    (reduce gather-join [] schema)))

(defn schema->attrs
  [model]
  (let [name       (bm/model-name model)
        schema     (bm/schema model)
        attr-names (gather-attr-names name schema)
        joins      (gather-joins model schema)
        ret        (into [] (concat [:db/id] attr-names joins))]
    ret))

;;;; Schema generation

(defn datascript-schema [[model attrs]]
  (letfn [(ref? [[attr-name attr-spec]]
            (some #{:ref} attr-spec))
          (ref-attr-schema [model [attr-name attr-spec]]
            [(keyword (name (bm/model-name model)) (name attr-name))
             (cond
               #(some #{:ref} attr-spec)
               (if (some #{:many} attr-spec)
                 {:db.valueType   :db.type/ref
                  :db/cardinality :db.cardinality/many}
                 {:db/valueType   :db.type/ref}))])]
    (into {}
          (comp (filter ref?)
                (map #(ref-attr-schema model %)))
          attrs)))
