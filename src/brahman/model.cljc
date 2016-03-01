(ns brahman.model
  (:require [com.rpl.specter :as s]))

;;;; Transformations

(defn transform-specter [tspec raw-data]
  (let [seq-data (if (coll? raw-data) raw-data [raw-data])
        tform    (into [] (conj tspec seq-data))
        res      (apply s/transform tform)]
    (cond-> res
      (not (coll? raw-data)) first)))

;;;; Model protocol

(defprotocol IModel
  (model-name [this] "The name of the model as a symbol")
  (schema     [this] "The raw schema as specified originally")
  (validation [this] "Validation rules defined for the model")
  (sources    [this] "Data sources used in this model")
  (stores     [this] "The stores used in the sources of this model")
  (attrs      [this] "Attributes for use in queries, computed from
                      the raw schema (with joins etc.)")
  (query      [this env q]
              [this env q extra]
                     "Queries the model based on its data sources,
                      given the query q and extra information (e.g.
                      query clauses).")
  (validate   [this data]))

;;; Source queries

(defn- query-store
  [{:keys [schema->attrs query-source model source] :as env} q extra]
  (let [attrs (or q (schema->attrs model))]
    (query-source env
                  (:query source)
                  {:inputs [attrs] :extra extra})))

(defn- query-derived-attr
  [{:keys [data store entity-id merge-attr
           query-source transform model] :as env} source]
  (into #{}
        (map (fn [entity]
               (let [id      (entity-id entity)
                     raw     (query-source env
                                           (:query source)
                                           {:inputs [id]})
                     tformed (cond->> raw
                               (:transform source)
                               (transform (:transform source)))]
                 (merge-attr (schema model) entity
                             (:name source) tformed))))
        data))

(defmulti ^:private query-sources (fn [_ _ _ _ _ [type _]] type))

(defmethod query-sources :store
  [model env q extra data [type sources]]
  (let [config       (:config model)
        merge-source (:merge-source config)]
    (reduce (fn [data source]
              (let [env'  (merge env config {:model  model
                                             :data   data
                                             :source source})
                    sdata (query-store env' q extra)]
                (merge-source source data sdata)))
            data
            sources)))

(defmethod query-sources :derived-attr
  [model env _ _ data [type sources]]
  (let [config (:config model)]
    (reduce (fn [ret source]
              (let [env' (merge env config {:model  model
                                            :data   data
                                            :source source})]
                (query-derived-attr env' source)))
            data
            sources)))

(defmethod query-sources :default
  [_ _ _ _ _ [type _]]
  (let [msg (str "Unknown source type: " type)]
    (throw #?(:cljs (js/Error. msg)
              :clj  (Exception. msg)))))

(defn- compare-source-types [t1 t2]
  (let [order (let [types [:store :derived-attr]]
                #?(:cljs (to-array types)
                   :clj  types))]
    (compare (.indexOf order t1) (.indexOf order t2))))

;;;; Model implementation

(defrecord Model [props config]
  IModel
  (model-name [this]
    (:name props))

  (schema [this]
    (:schema props))

  (validation [this]
    (:validation props))

  (sources [this]
    (:sources props))

  (stores [this]
    (set (remove nil? (map :store (sources this)))))

  (attrs [this]
    ((:schema->attrs config) this))

  (query [this env q]
    (query this env q nil))

  (query [this env q extra]
    (let [sources (sources this)
          sorted  (sort-by :type compare-source-types sources)
          grouped (group-by :type sorted)]
      (reduce #(query-sources this env q extra %1 %2)
              nil
              grouped)))

  (validate [this data]
    (let [config (:config this)]
      ((:validate config) this (validation this) data))))

(defn model
  [{:keys [schema] :as props} config]
  {:pre [(map? schema)]}
  (Model. props config))

;;;; Modeler protocol

(defprotocol IModeler
  (get-model [this name] "Returns the model with the given name")
  (schemas   [this]      "Returns a map of the following structure:
                          {<store1 name> {<model1 name> <schema>
                                          <model2 name> <schema>}
                           ...}"))

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

(defrecord Modeler [config]
  IModeler
  (get-model [this name]
    (let [models (:models config)]
      (first (filter #(= name (model-name %)) models))))

  (schemas [this]
    (let [models (:models config)
          stores (set (apply concat (map stores models)))]
      (letfn [(collect-step [res store]
                (let [models  (models-for-store models store)
                      schemas (map schema models)
                      m       (zipmap models schemas)]
                  (update res store #(into {} (merge % m)))))]
        (reduce collect-step {} stores)))))

(defn- default-store-schema [schema]
  schema)

(defn- default-validation [model]
  nil)

(defn- default-schema->attrs [model]
  [])

(defn- default-validate [model data]
  true)

(defn- default-query-source
  [env query {:keys [inputs extra]}]
  nil)

(defn- default-transform [tspec raw-value]
  (tspec raw-value))

(defn- default-merge-source
  [source data source-data]
  (-> #{}
      (into data)
      (into source-data)))

(defn modeler
  [{:keys [models
           merge-model
           install-schemas
           schema->attrs
           validate
           entity-id
           query-source
           merge-source
           transform]
    :or {models          []
         merge-model     default-merge-model
         install-schemas (constantly nil)
         schema->attrs   default-schema->attrs
         validate        default-validate
         entity-id       identity
         query-source    default-query-source
         merge-source    default-merge-source
         transform       default-transform}
    :as config}]
  {:pre [(map? config)]}
  (let [merged-models (merge-models merge-model models)
        config'       {:models          merged-models
                       :install-schemas install-schemas
                       :entity-id       entity-id
                       :validate        validate
                       :schema->attrs   schema->attrs
                       :query-source    query-source
                       :merge-source    merge-source
                       :transform       transform}
        models'       (for [model-spec merged-models]
                        (model model-spec config'))
        modeler'      (Modeler. (assoc config' :models models'))]
    (install-schemas! modeler')
    modeler'))
