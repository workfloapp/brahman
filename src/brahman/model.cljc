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
  (query      [this q]
              [this q extra]
                     "Queries the model based on its data sources,
                      given the query q and extra information (e.g.
                      query clauses).")
  (validate   [this data]))

;;; Source queries

(defn- query-main
  [{:keys [store schema->attrs query model]} source q extra]
  (let [attrs (or q (schema->attrs model))]
    (query {:store store :model model}
           (:query source)
           {:inputs [attrs] :extra extra})))

(defn- query-derived-attr
  [{:keys [data store entity-id merge-attr
           query transform model]} source]
  (mapv (fn [entity]
          (let [id      (entity-id entity)
                raw     (query {:store store :model model}
                               (:query source)
                               {:inputs [id]})
                tformed (cond->> raw
                          (:transform source)
                          (transform (:transform source)))]
            (merge-attr (schema model) entity
                        (:name source) tformed)))
        data))

(defmulti ^:private query-sources (fn [_ _ _ _ [type _]] type))

(defmethod query-sources :main
  [model q extra data [type sources]]
  (let [config     (:config model)
        merge-main (:merge-main config)]
    (reduce (fn [data source]
              (let [env   (merge config {:model model :data data})
                    sdata (query-main env source q extra)]
                (merge-main data sdata)))
            data
            sources)))

(defmethod query-sources :derived-attr
  [model _ _ ret [type sources]]
  (let [config (:config model)]
    (reduce (fn [ret source]
              (let [env (merge config {:model model :data ret})]
                (query-derived-attr env source)))
            ret sources)))

(defmethod query-sources :default
  [_ _ _ _ [type _]]
  (let [msg (str "Unknown source type: " type)]
    (throw #?(:cljs (js/Error. msg)
              :clj  (Exception. msg)))))

(defn- compare-source-types [t1 t2]
  (let [order [:main :derived-attr]]
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

  (query [this q]
    (query this q nil))

  (query [this q extra]
    (let [sources (sources this)
          sorted  (sort-by :type compare-source-types sources)
          grouped (group-by :type sorted)]
      (reduce #(query-sources this q extra %1 %2)
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
  (get-model [this name]))

;;;; Modeler implementation

(defn- models-for-store [models store]
  (into #{}
        (filter (fn [model]
                  (some #{store} (stores model))))
        models))

(defn- collect-schemas
  "Returns a map of the following structure:

   {<store1 name> {<model1 name> <schema>
                   <model2 name> <schema>}
    ...}"
  [models]
  (letfn [(collect-step [res store]
            (let [models  (models-for-store models store)
                  schemas (map schema models)
                  m       (zipmap models schemas)]
              (update res store #(into {} (merge % m)))))]
    (let [stores (set (apply concat (map stores models)))]
      (reduce collect-step {} stores))))

(defn- install-schemas!
  "Collects the schemas from all models"
  [modeler]
  (let [config  (:config modeler)
        models  (:models config)]
    ((:install-schemas config) (collect-schemas models))))

(defrecord Modeler [config]
  IModeler
  (get-model [this name]
    (let [models (:models config)]
      (first (filter #(= name (model-name %)) models)))))

(defn- default-store-schema [schema]
  schema)

(defn- default-validation [model]
  nil)

(defn- default-schema->attrs [model]
  [])

(defn- default-validate [model data]
  true)

(defn- default-query-source
  [store model query {:keys [inputs extra]}]
  store)

(defn- default-transform [tspec raw-value]
  (tspec raw-value))

(defn modeler
  [{:keys [models
           install-schemas
           schema->attrs
           validate
           entity-id
           query-source
           transform]
    :or {models          []
         install-schemas (constantly nil)
         schema->attrs   default-schema->attrs
         validate        default-validate
         entity-id       identity
         query-source    default-query-source
         transform       default-transform}
    :as config}]
  {:pre [(map? config)]}
  (let [config'  {:models          models
                  :install-schemas install-schemas
                  :entity-id       entity-id
                  :validate        validate
                  :schema->attrs   schema->attrs
                  :query-source    query-source
                  :transform       transform}
        models'  (for [model-spec models]
                   (model model-spec config'))
        modeler' (Modeler. (assoc config' :models models'))]
    (install-schemas! modeler')
    modeler'))
