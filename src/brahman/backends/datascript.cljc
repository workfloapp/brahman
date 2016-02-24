(ns brahman.backends.datascript
  (:require [brahman.model :as bm]))

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
