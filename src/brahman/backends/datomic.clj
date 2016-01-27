(ns brahman.backends.datomic
  (:require [datomic.api :as d]
            [datomic-schema.schema :as s]
            [brahman.model :as bm]))

;;;; Schema attribute extraction

(defn- attr-name [schema-name short-name]
  (keyword (name schema-name) (name short-name)))

(defn- join? [[short-name spec]]
  (and (map? spec) (= 1 (count spec))))

(defn- gather-attr-names [model-name schema]
  {:pre [(map? schema)
         (contains? schema :attrs)
         (map? (:attrs schema))]}
  (into []
        (comp (remove join?)
              (map (fn [[name attr-spec]]
                     (attr-name model-name name))))
        (:attrs schema)))

(declare schema->attrs)

(defn- gather-joins [model-name schema]
  {:pre [(map? schema)
         (contains? schema :name)
         (contains? schema :attrs)
         (map? (:attrs schema))]}
  (letfn [(join-attr [schema-name short-name spec]
            (let [[spec target] (first spec)]
              (hash-map (attr-name schema-name short-name)
                        (cond-> target
                          (not= target '...) schema->attrs))))
          (gather-join [res [short-name spec]]
            (cond-> res
              (map? spec) (conj (join-attr model-name
                                           short-name
                                           spec))))]
    (reduce gather-join [] (:attrs schema))))

(defn schema->attrs
  [model]
  (let [name       (bm/model-name model)
        schema     (bm/schema model)
        attr-names (gather-attr-names name schema)
        joins      (gather-joins name schema)
        ret (into [] (concat [:db/id] attr-names joins))]
    ret))

;;;; Schema generation

(defn- kv->field [res [k v]]
  (cond
    (vector? v) (conj res (into [] (concat [k] v)))
    (map? v)    (kv->field res [k (ffirst v)])
    :else       (conj res [k v])))

(defn- attrs->fields [schema]
  (reduce kv->field [] schema))

(defn datomic-schema [[model attrs]]
  (let [fields (attrs->fields attrs)]
    (eval `(s/schema ~(bm/model-name model)
                     (s/fields ~@fields)))))

;;;; Schema installation

(defn install-schemas
  [conn schemas]
  (let [dschemas (mapv datomic-schema schemas)]
    (->> (concat (s/generate-parts [(s/part "app")])
                 (s/generate-schema dschemas))
         (d/transact conn)
         (deref))))
