(ns brahman.backends.datomic
  (:require [datomic.api :as d]
            [datomic-schema.schema :as s]
            [brahman.backends.datascript :as bds]
            [brahman.model :as bm]))

;;;; Schema attribute extraction

(def model->attrs bds/model->attrs)

;;;; Schema generation

(defn datomic-schema [[model attrs]]
  (let [fields (bds/attrs->fields attrs)]
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
