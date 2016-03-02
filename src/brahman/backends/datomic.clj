(ns brahman.backends.datomic
  (:require [datomic.api :as d]
            [datomic-schema.schema :as s]
            [brahman.backends.datascript :as bds]
            [brahman.model :as bm]))

;;;; Schema attribute extraction

(def schema->attrs bds/schema->attrs)

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
