(ns brahman.test.model
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.rpl.specter :as specter :refer :all]
            [brahman.model :as bm]))

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

(defspec models-satisfy-ibrahman
  (prop/for-all [schema-names (gen/vector gen/symbol)]
    (let [specs   (mapv (fn [schema-name]
                          {:schema {:name schema-name}})
                        schema-names)
          modeler (bm/modeler {:models specs})
          models  (mapv #(bm/get-model modeler %) schema-names)]
      (and (is (every? #(not (nil? %)) models))
           (is (every? #(satisfies? bm/IModel %) models))
           (is (= (into #{} schema-names)
                  (into #{} (map bm/model-name) models)))
           (is (= (into #{} (map :schema) specs)
                  (into #{} (map bm/schema) models)))))))

(defspec models-have-the-same-name-as-their-schemas
  (prop/for-all [schema-name gen/symbol]
    (let [spec    {:schema {:name schema-name}}
          modeler (bm/modeler {:models [spec]})
          model   (bm/get-model modeler schema-name)]
      (and (is (not (nil? model)))
           (is (= schema-name (bm/model-name model)))))))

(defspec models-remember-their-schema
  (prop/for-all [schema-name gen/symbol]
    (let [spec    {:schema {:name schema-name}}
          modeler (bm/modeler {:models [spec]})
          model   (bm/get-model modeler schema-name)]
      (and (is (not (nil? model)))
           (is (= (:schema spec) (bm/schema model)))))))

;;;; Simple data models for regular collections

(defspec vectors-can-be-used-as-dbs 10
  (prop/for-all [values (gen/vector gen/any)]
    (let [item    {:schema {:name 'item}
                   :sources [{:type :main}]}
          modeler (bm/modeler
                    {:models [item]
                     :store values
                     :query (fn [{:keys [store]} _ _] store)})
          items   (bm/get-model modeler 'item)]
      (is (= values (bm/query items nil nil))))))

(defspec queries-can-be-used-to-select-keys 10
  (prop/for-all [values (gen/vector (gen/map
                                     gen/simple-type
                                     gen/simple-type
                                     {:num-elements 200})
                                    10)]
    (let [item        {:schema {:name 'item}
                       :sources [{:type :main}]}
          modeler     (bm/modeler
                        {:models [item]
                         :store values
                         :query (fn [{:keys [store]} _ {:keys [inputs]}]
                                  (let [keys (first inputs)]
                                    (mapv #(select-keys % keys) store)))})
          items       (bm/get-model modeler 'item)
          common-keys (reduce clojure.set/intersection
                              (into #{} (keys (first values)))
                              (map (comp (partial into #{}) keys)
                                   (rest values)))]
      (is (= (into #{} (map #(select-keys % common-keys)) values)
             (into #{} (bm/query nil items (into [] common-keys))))))))

(defspec extracting-validation-rules-works 10
  (prop/for-all [attrs (gen/map
                        gen/keyword
                        (gen/vector gen/simple-type))]
    (let [item        {:schema {:name 'item
                                :attrs attrs}
                       :sources [{:type :main}]}
          ;; This validation extractor function assumes
          ;; the validation rules for each attribute is
          ;; the first keyword in each attribute spec.
          ;; It returns a map with the structure
          ;; {<attr1 name> <first key of attr1 spec>
          ;;  <attr2 name> <first key of attr2 spec>}
          extract-fn  (fn [attrs]
                        (into {}
                              (map (fn [[name spec]]
                                     [name (first spec)]))
                              attrs))
          expected    (extract-fn attrs)
          modeler     (bm/modeler
                        {:models [item]
                         :validation (fn [schema]
                                       (extract-fn (:attrs schema)))})
          items       (bm/get-model modeler 'item)]
      (is (= (into #{} expected)
             (into #{} (bm/validation items)))))))
