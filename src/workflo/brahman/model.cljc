(ns workflo.brahman.model
  (:require [com.rpl.specter :as s]
            [om.next.impl.parser :as om-parser]))

;;;; Transformations

(defn transform-specter [tspec raw-data]
  (let [seq-data (if (coll? raw-data) raw-data [raw-data])
        tform    (into [] (conj tspec seq-data))
        res      (apply s/transform tform)]
    (cond-> res
      (not (coll? raw-data)) first)))

;;;; Model protocol

(defprotocol IModel
  (model-name    [this] "The name of the model as a symbol")
  (schema        [this] "The raw schema as specified originally")
  (validation    [this] "Validation rules defined for the model")
  (stores        [this] "The data stores used in this model")
  (derived-attrs [this] "The derived attributes used in this model")
  (attrs         [this]
                 [this recursive?]
                        "Attributes for use in queries, computed
                         from the raw schema (with joins etc.)")
  (get-modeler   [this] "Returns the modeler that manages the
                         models.")
  (query         [this q]
                 [this q env]
                 [this q env params]
                        "Queries the model based on its data
                         stores and derived attributes, given the
                         query q and extra information (e.g. query
                         clauses).")
  (validate      [this data]))

;;;; Modeler protocol

(defprotocol IModeler
  (models    [this]      "Returns all models known to the modeler.")
  (get-model [this name] "Returns the model with the given name")
  (schemas   [this]      "Returns a map of the following structure:
                          {<store1 name> {<model1 name> <schema>
                                          <model2 name> <schema>}
                           ...}"))

;;;; Utilities

(defn collection?
  [query-result]
  (and (coll? query-result)
       (not (map? query-result))))

(defn derived-attr-name
  [model attr]
  (if (or (:prefixed? attr)
          (not (some #{:prefixed?} (keys attr))))
    (keyword (name (model-name model))
             (name (:name attr)))
    (keyword (namespace (:name attr))
             (name (:name attr)))))

;;;; Query execution

(defn query-derived-attr
  [{:keys [merge-derived-attr model query-derived-attr] :as env}
   attr attr-q result]
  (letfn [(derive-attr [entity]
            (->> (query-derived-attr env attr attr-q entity)
                 (merge-derived-attr model entity attr)))]
    (if (collection? result)
      (into #{} (map derive-attr result))
      (derive-attr result))))

(defn has-join-attr?
  [query-result attr-name]
  (if (collection? query-result)
    (contains? (first query-result) attr-name)
    (contains? query-result attr-name)))

(declare query-derived-attrs)
(declare extract-attr-query)

(defn query-derived-attrs-follow-joins
  [model env ast query-result]
  (let [config (:config model)
        joins  ((:model->joins config) model)]
    (reduce (fn [query-result [attr-name attr :as join]]
              (letfn [(follow-join [entity-or-entities]
                        (let [attr-query (extract-attr-query
                                          ast attr-name)]
                          (query-derived-attrs (:model attr) env
                                               attr-query
                                               entity-or-entities)))]
                (if (has-join-attr? query-result attr-name)
                  (if (collection? query-result)
                    (into #{}
                          (map #(update % attr-name follow-join))
                          query-result)
                    (update query-result attr-name follow-join))
                  query-result)))
            query-result
            joins)))

(defn extract-attr-query
  [ast attr-name]
  (first (filter #(= attr-name (:key %)) (:children ast))))

(defn query-derived-attrs
  [model env ast query-result]
  (let [config (:config model)
        result (reduce (fn [query-result {:keys [name] :as attr}]
                         (let [env'   (merge env config {:model model})
                               attr-n (derived-attr-name model attr)
                               attr-q (extract-attr-query ast attr-n)]
                           (cond->> query-result
                             (not (nil? attr-q))
                             (query-derived-attr env' attr
                                                 (:query attr-q)))))
                       query-result
                       (derived-attrs model))
        result (query-derived-attrs-follow-joins model env ast result)]
    result))

(defn query-store
  [{:keys [model->attrs query-store model] :as env} q params]
  (let [attrs (or q (model->attrs model false))]
    (query-store env attrs params)))

(defn query-stores
  [model env q params]
  (let [config      (:config model)
        merge-store (:merge-store config)]
    (reduce (fn [ret store]
              (let [env'   (assoc (merge env config)
                                  :model model
                                  :store store)
                    result (query-store env' q params)]
                (if (:fetch-one? env)
                  (merge ret result)
                  (merge-store store ret result))))
            nil
            (stores model))))

;;;; Model implementation

(defrecord Model [props config modeler]
  IModel
  (model-name [this]
    (:name props))

  (schema [this]
    (:schema props))

  (validation [this]
    (:validation props))

  (stores [this]
    (:stores props))

  (derived-attrs [this]
    (:derived-attrs props))

  (attrs [this]
    (attrs this true))

  (attrs [this recursive?]
    ((:model->attrs config) this recursive?))

  (get-modeler [this]
    modeler)

  (query [this q]
    (query this q {}))

  (query [this q env]
    (query this q env []))

  (query [this q env params]
    (let [ast (om-parser/query->ast q)]
      (->> (query-stores this env q params)
           (query-derived-attrs this env ast))))

  (validate [this data]
    (let [config (:config this)]
      ((:validate config) this (validation this) data))))

(defn model
  [{:keys [schema] :as props} config modeler]
  {:pre [(or (nil? schema) (map? schema))
         (satisfies? IModeler modeler)]}
  (Model. props config modeler))

;;;; Model merging

(defn merge-model-field
  [field1 field2]
  (cond
    (and (vector? field1) (vector? field2)) (concat field1 field2)
    (and (map? field1) (map? field2))       (merge field1 field2)
    (= field1 field2)                       field1))

(defn default-merge-model
  "Merges a model into a resulting model."
  [res model]
  (merge-with merge-model-field res model))

(defn merge-model-group
  [merge-model [name group]]
  (reduce merge-model {} group))

(defn merge-models
  "Merges all models with the same name."
  [merge-model models]
  (let [grouped (group-by :name models)]
    (mapv #(merge-model-group merge-model %) grouped)))

;;;; Modeler implementation

(defn- models-for-store [models store]
  (into #{}
        (filter (fn [model]
                  (some #{store} (stores model))))
        models))

(defn- install-schemas!
  "Collects the schemas from all models"
  [modeler]
  (let [config (:config modeler)]
    ((:install-schemas config) (schemas modeler))))

(defrecord Modeler [config models]
  IModeler
  (models [this]
    @models)

  (get-model [this name]
    (first (filter #(= name (model-name %)) @models)))

  (schemas [this]
    (let [models @models]
      (let [stores (set (apply concat (map stores models)))]
        (letfn [(collect-step [res store]
                  (let [models  (models-for-store models store)
                        schemas (map schema models)
                        m       (zipmap models schemas)]
                    (update res store #(into {} (merge % m)))))]
          (reduce collect-step {} stores))))))

(defn- default-store-schema
  [schema]
  schema)

(defn- default-validation
  [model]
  nil)

(defn- default-model->attrs
  [model recursive?]
  [])

(defn- default-model->joins
  [model]
  {})

(defn- default-validate
  [model validation data]
  nil)

(defn- default-query-store
  [env query {:keys [inputs extra]}]
  nil)

(defn- default-query-derived-attr
  [env attr query entity]
  nil)

(defn- default-query-link
  [env link query]
  nil)

(defn- default-transform [tspec raw-value]
  (tspec raw-value))

(defn- default-merge-store
  [store data store-data]
  (-> #{}
      (into data)
      (into store-data)))

(defn- default-merge-derived-attr
  [model entity attr value]
  (let [attr-name (derived-attr-name model attr)]
    (assoc entity attr-name value)))

(defn modeler
  [{:keys [models
           merge-model
           install-schemas
           model->attrs
           model->joins
           validate
           entity-id
           query-store
           query-derived-attr
           query-link
           merge-store
           merge-derived-attr
           transform]
    :or {models             []
         merge-model        default-merge-model
         install-schemas    (constantly nil)
         model->attrs       default-model->attrs
         model->joins       default-model->joins
         validate           default-validate
         entity-id          :db/id
         query-store        default-query-store
         query-derived-attr default-query-derived-attr
         query-link         default-query-link
         merge-store        default-merge-store
         merge-derived-attr default-merge-derived-attr
         transform          default-transform}
    :as config}]
  {:pre [(map? config)]}
  (let [config        {:install-schemas    install-schemas
                       :entity-id          entity-id
                       :validate           validate
                       :model->attrs       model->attrs
                       :model->joins       model->joins
                       :query-store        query-store
                       :query-derived-attr query-derived-attr
                       :query-link         query-link
                       :merge-store        merge-store
                       :merge-derived-attr merge-derived-attr
                       :transform          transform}
        model-specs   (merge-models merge-model models)
        models        (atom #{})
        modeler       (Modeler. config models)]
    (doseq [model-spec model-specs]
      (let [model' (model model-spec config modeler)]
        (swap! models conj model')))
    (install-schemas! modeler)
    modeler))
