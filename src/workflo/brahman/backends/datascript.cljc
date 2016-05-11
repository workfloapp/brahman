(ns workflo.brahman.backends.datascript
  (:require [datascript.core :as d]
            [workflo.brahman.model :as bm]))

;;;; Schema attribute extraction

(defn- attr-name
  "Takes a a schema name 'foo' and a short attribute name 'bar' and
   returns a prefixed attribute name as a keyword (e.g. :foo/bar)."
  [schema-name short-name]
  (keyword (name schema-name) (name short-name)))

(defn- join?
  "Takes an attribute name and attribute specification name and
   returns whether or not the specification is a join with another
   model."
  [[short-name spec]]
  (and (map? spec)
       (= 1 (count spec))))

(defn- gather-regular-attr-names
  "Returns a vector of the prefixed names of all regular, non-join
   attributes in the model."
  [model-name schema]
  {:pre [(map? schema)]}
  (into []
        (comp (remove join?)
              (map (fn [[name attr-spec]]
                     (attr-name model-name name))))
        schema))

(declare model->attrs)

(defn model->joins
  "Takes a model and returns a map of model-prefixed attribute names
   to {:model <target model> :attr <original attr spec>} map. It does
   not recurse into target models and their attributes."
  [model]
  (letfn [(join [model-name short-name attr]
            (let [[spec target] (first attr)
                  modeler      (bm/get-modeler model)
                  target-model  (if (= target '...)
                                  model
                                  (bm/get-model modeler target))]
              (hash-map (attr-name model-name short-name)
                        {:model target-model
                         :attr  attr})))
          (gather-join [res [short-name spec :as attr]]
            (cond-> res
              (join? attr) (merge (join (bm/model-name model)
                                        short-name
                                        spec))))]
    (reduce gather-join {} (bm/schema model))))

(defn- gather-join-attrs
  "Returns a vector of all join attributes in the model, each
   represented by a {<prefixed attr name> <target model attributes>}
   map. Resolves joins recursively to fill in the target model
   attributes."
  [model recursive?]
  (let [schema (bm/schema model)]
    (letfn [(recur-join [target]
              (model->attrs target recursive?))
            (join-attr [[attr-name attr-info]]
              (hash-map attr-name
                        (if (and recursive?
                                 (not= (:model attr-info) model))
                          (recur-join (:model attr-info))
                          [:db/id])))]
      (into [] (map join-attr) (model->joins model)))))

(defn model->attrs
  "Returns a vector of all attributes in the model, with joins
   resolved into the target models recursively."
  [model recursive?]
  (let [name       (bm/model-name model)
        schema     (bm/schema model)
        attr-names (gather-regular-attr-names name schema)
        joins      (gather-join-attrs model recursive?)
        ret        (into [] (concat [:db/id] attr-names joins))]
    ret))

;;;; Schema generation

(defn kv->field
  [res [k v]]
  (cond
    (vector? v) (conj res (into [] (concat [k] v)))
    (map? v)    (kv->field res [k (ffirst v)])
    :else       (conj res [k v])))

(defn attrs->fields
  [schema]
  (reduce kv->field [] schema))

(defn datascript-schema
  [[model attrs]]
  (letfn [(ref? [[attr-name & attr-spec]]
            (some #{:ref} attr-spec))
          (ref-attr-schema [model [attr-name & attr-spec]]
            (let [attr-schema [(keyword (name (bm/model-name model))
                                        (name attr-name))
                               (cond
                                 #(some #{:ref} attr-spec)
                                 (if (some #{:many} attr-spec)
                                   {:db.valueType   :db.type/ref
                                    :db/cardinality :db.cardinality/many}
                                   {:db/valueType   :db.type/ref}))]]
              attr-schema))]
    (into {}
          (comp (filter ref?)
                (map #(ref-attr-schema model %)))
          (attrs->fields attrs))))

;;;; Schema installation

(defn install-schemas
  [stores schemas]
  (let [matching-schemas (->> (select-keys schemas stores)
                              (vals)
                              (apply concat)
                              (mapv datascript-schema)
                              (apply merge))]
    (d/create-conn matching-schemas)))

;;;; Query execution

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
          (bm/attrs model false))))

(defn datascript-query
  "Translate an Om Next query to a DataScript query based
   on the current DataScript schema."
  ([env query]
   (datascript-query env query []))
  ([{:keys [model fetch-one?]} query clauses]
   (let [model-attr (identifying-model-attr model)]
     (if fetch-one?
       ;; Query a single entity
       `[:find (~'pull ~'?e ~query) .
         :where [~'?e ~model-attr]
                ~@clauses]
       ;; Query a collection of entities
       `[:find [(~'pull ~'?e ~query)]
         :where [~'?e ~model-attr]]))))

(defn query-store
  "Execute an Om Next query against DataScript."
  [conn env query clauses]
  (let [ds-query  (datascript-query env query clauses)
        ds-result (d/q ds-query @conn)]
    ds-result))
