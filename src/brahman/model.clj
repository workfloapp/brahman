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
  (model-name   [this] "The name of the model as a symbol")
  (schema       [this] "The raw schema as specified originally")
  (store-schema [this] "The schema parsed for the store being used")
  (validation   [this] "Validation data parsed from the raw schema")
  (sources      [this] "Data sources used in this model")
  (query-attrs  [this] "Attributes for use in queries, computed from
                        the raw schema (with joins etc.)")
  (query        [this q]
                [this q extra]
                       "Queries the model based on its data sources,
                        given the query q and extra information (e.g.
                        query clauses)."))

;;; Source queries

(defn- query-main
  [{:keys [store query-attrs query model]} source q extra]
  (let [attrs (or q (query-attrs model))]
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
  (throw (Exception. (str "Unknown source type: " type))))

(defn- compare-source-types [t1 t2]
  (let [order [:main :derived-attr]]
    (compare (.indexOf order t1) (.indexOf order t2))))

;;;; Model implementation

(defrecord Model [props config]
  IModel
  (model-name [this]
    (:name (schema this)))

  (schema [this]
    (:schema props))

  (store-schema [this]
    ((:store-schema config) (schema this)))

  (validation [this]
    ((:validation config) (schema this)))

  (sources [this]
    (:sources props))

  (query-attrs [this]
    ((:query-attrs config) this))

  (query [this q]
    (query this q nil))

  (query [this q extra]
    (let [sources (sources this)
          sorted  (sort-by :type compare-source-types sources)
          grouped (group-by :type sorted)]
      (reduce #(query-sources this q extra %1 %2)
              nil
              grouped))))

(defn model
  [{:keys [schema] :as props} config]
  {:pre [(and (map? schema) (contains? schema :name))]}
  (Model. props config))

;;;; Modeler protocol

(defprotocol IModeler
  (get-model [this name]))

;;;; Modeler implementation

(defn- install-schemas! [modeler]
  (let [config  (:config modeler)
        store   (:store config)
        models  (:models config)
        schemas (mapv store-schema models)]
    ((:install-schemas config) store schemas)))

(defrecord Modeler [config]
  IModeler
  (get-model [this name]
    (let [models (:models config)]
      (first (filter #(= name (model-name %)) models)))))

(defn- default-store-schema [schema]
  schema)

(defn- default-validation [model]
  nil)

(defn- default-query-attrs [model]
  [])

(defn- default-query
  [store model query {:keys [inputs extra]}]
  store)

(defn- default-transform [tspec raw-value]
  (tspec raw-value))

(defn- default-merge-main [mains main]
  (into [] (concat mains main)))

(defn- default-merge-attr [schema entity name value]
  (assoc entity name value))

(defn modeler
  [{:keys [models
           store store-schema install-schemas
           entity-id
           validation
           query-attrs query
           transform
           merge-main merge-attr]
    :or {models          []
         store           nil
         store-schema    default-store-schema
         install-schemas (constantly nil)
         entity-id       identity
         validation      default-validation
         query-attrs     default-query-attrs
         query           default-query
         transform       default-transform
         merge-main      default-merge-main
         merge-attr      default-merge-attr}
    :as config}]
  {:pre [(map? config)]}
  (let [config'  {:models          models
                  :store           store
                  :store-schema    store-schema
                  :install-schemas install-schemas
                  :entity-id       entity-id
                  :validation      validation
                  :query-attrs     query-attrs
                  :query           query
                  :transform       transform
                  :merge-main      merge-main
                  :merge-attr      merge-attr}
        models'  (for [model-spec models]
                   (model model-spec config'))
        modeler' (Modeler. (assoc config' :models models'))]
    (install-schemas! modeler')
    modeler'))
