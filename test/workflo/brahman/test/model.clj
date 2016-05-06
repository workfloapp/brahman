(ns workflo.brahman.test.model
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.rpl.specter :as specter :refer :all]
            [workflo.brahman.model :as bm]))

;;;; Specter transformations

(defspec transform-specter-works-on-vectors
  (prop/for-all [numbers (gen/vector gen/nat)]
    (is (= (mapv inc numbers)
           (bm/transform-specter [[ALL] inc] numbers)))))

(defspec transform-specter-works-on-maps
  (prop/for-all [numbers (gen/map gen/string gen/nat)]
    (is (= (zipmap (keys numbers) (map dec (vals numbers)))
           (bm/transform-specter [[ALL LAST] dec] numbers)))))

(defspec transform-specter-works-on-scalars
  (prop/for-all [val gen/simple-type]
    (is (= [:x val]
           (bm/transform-specter [[ALL] #(conj [:x] %)] val)))))

;;;; Simple properties / IModel interface functions

(defspec models-satisfy-imodel
  (prop/for-all [model-names (gen/vector gen/symbol)]
    (let [specs   (mapv (fn [schema-name]
                          {:name   schema-name
                           :schema {}})
                        model-names)
          modeler (bm/modeler {:models specs})
          models  (mapv #(bm/get-model modeler %) model-names)]
      (and (is (every? #(not (nil? %)) models))
           (is (every? #(satisfies? bm/IModel %) models))
           (is (= (into #{} model-names)
                  (into #{} (map bm/model-name) models)))
           (is (= (into #{} (map :schema) specs)
                  (into #{} (map bm/schema) models)))))))

(defspec models-remember-their-schema
  (prop/for-all [model-name gen/symbol
                 schema     (gen/map gen/keyword gen/keyword)]
    (let [spec    {:name   model-name
                   :schema schema}
          modeler (bm/modeler {:models [spec]})
          model   (bm/get-model modeler model-name)]
      (and (is (not (nil? model)))
           (is (= (:schema spec)
                  (bm/schema model)))))))

;;;; Simple data models for regular collections

(defspec vectors-can-be-used-as-dbs 10
  (prop/for-all [values (gen/vector gen/any)]
    (let [spec    {:name   'item
                   :stores [:vector]}
          modeler (bm/modeler
                    {:models [spec]
                     :query-store (fn [{:keys [store]} _ _]
                                    (case store :vector values))})
          items   (bm/get-model modeler 'item)]
      (is (= (into #{} values) (bm/query items nil))))))

(defspec queries-can-be-used-to-select-keys 10
  (prop/for-all [values (gen/vector (gen/map gen/simple-type
                                             gen/simple-type
                                             {:num-elements 200})
                                    10)]
    (let [spec        {:name   'item
                       :stores [:vector]}
          modeler     (bm/modeler
                        {:models [spec]
                         :query-store
                         (fn [{:keys [store]} q _]
                           (case store
                             :vector (mapv #(select-keys % q) values)))})
          items       (bm/get-model modeler 'item)
          common-keys (reduce clojure.set/intersection
                              (into #{} (keys (first values)))
                              (map (comp (partial into #{}) keys)
                                   (rest values)))]
      (is (= (into #{} (map #(select-keys % common-keys)) values)
             (into #{} (bm/query items (into [] common-keys))))))))
