(ns brahman.backends.datascript
  (:require [brahman.model :as bm]))

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
  (let [schema (bm/schema model)]
    (letfn [(join [schema-name short-name attr]
              (let [[spec target] (first attr)
                    modeler       (bm/get-modeler model)
                    target-name   (if (= target '...) model target)
                    target-model  (bm/get-model modeler target-name)]
                (hash-map (attr-name schema-name short-name)
                          {:model target-model
                           :attr  attr})))
            (gather-join [res [short-name spec :as attr]]
              (cond-> res
                (join? attr) (merge (join (bm/model-name model)
                                          short-name
                                          spec))))]
      (reduce gather-join {} schema))))

(defn- gather-join-attrs
  "Returns a vector of all join attributes in the model, each
   represented by a {<prefixed attr name> <target model attributes>}
   map. Resolves joins recursively to fill in the target model
   attributes."
  [model]
  (let [schema (bm/schema model)]
    (letfn [(recur-join [target]
              (model->attrs target))
            (join-attr [attr-name {:keys [model :as target attr]}]
              (hash-map attr-name
                        (cond-> target
                          (not= target model) recur-join)))]
      (reduce join-attr [] (model->joins model)))))

(defn model->attrs
  "Returns a vector of all attributes in the model, with joins
   resolved into the target models recursively."
  [model]
  (let [name       (bm/model-name model)
        schema     (bm/schema model)
        attr-names (gather-regular-attr-names name schema)
        joins      (gather-join-attrs model)
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
