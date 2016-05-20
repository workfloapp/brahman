(ns workflo.brahman.backends.datascript
  (:require [datascript.core :as d]
            [om.next.impl.parser :as om-parser]
            [workflo.brahman.model :as bm]))

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
  (letfn [(join [model-name short-name attr]
            (let [[spec target] (first attr)
                  modeler      (bm/get-modeler model)
                  target-model  (if (= target '...)
                                  model
                                  (bm/get-model modeler target))]
              (hash-map (attr-name model-name short-name)
                        {:model target-model
                         :attr  attr})))
          (gather-join [res [short-name spec :as attr]]
            (cond-> res
              (join? attr) (merge (join (bm/model-name model)
                                        short-name
                                        spec))))]
    (reduce gather-join {} (bm/schema model))))

(defn- gather-join-attrs
  "Returns a vector of all join attributes in the model, each
   represented by a {<prefixed attr name> <target model attributes>}
   map. Resolves joins recursively to fill in the target model
   attributes."
  [model recursive?]
  (let [schema (bm/schema model)]
    (letfn [(recur-join [target]
              (model->attrs target recursive?))
            (join-attr [[attr-name attr-info]]
              (hash-map attr-name
                        (if (and recursive?
                                 (not= (:model attr-info) model))
                          (recur-join (:model attr-info))
                          [:db/id])))]
      (into [] (map join-attr) (model->joins model)))))

(defn model->attrs
  "Returns a vector of all attributes in the model, with joins
   resolved into the target models recursively."
  [model recursive?]
  (let [name       (bm/model-name model)
        schema     (bm/schema model)
        attr-names (gather-regular-attr-names name schema)
        joins      (gather-join-attrs model recursive?)
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

;;;; Schema installation

(defn install-schemas
  [stores schemas]
  (let [matching-schemas (->> (select-keys schemas stores)
                              (vals)
                              (apply concat)
                              (mapv datascript-schema)
                              (apply merge))]
    (d/create-conn matching-schemas)))

;;;; Query execution

(defn query-derived-attr
  [conn env attr query entity]
  (let [entity-id (or (:entity-id env) :db/id)]
    (d/q (:query attr) @conn (entity-id entity) query)))

(defn- identifying-model-attr
  [model]
  (first (filter
          (fn [attr]
            (let [attr-name (cond-> attr
                              (map? attr) ffirst)]
              (= (name (bm/model-name model))
                 (namespace attr-name))))
          (bm/attrs model false))))

(defn datascript-query
  "Translate an Om Next query to a DataScript query based
   on the current DataScript schema."
  ([env query]
   (datascript-query env query []))
  ([{:keys [model fetch-one?]} query clauses]
   (let [model-attr (identifying-model-attr model)]
     (if fetch-one?
       ;; Query a single entity
       `[:find (~'pull ~'?e ~query) .
         :where [~'?e ~model-attr]
                ~@clauses]
       ;; Query a collection of entities
       `[:find [(~'pull ~'?e ~query) ~'...]
         :where [~'?e ~model-attr]
                ~@clauses]))))

(defn extract-links
  "Takes an Om Next query and extracts all links from it, returning
   the Om Next query without links and a mapping of query paths to
   links (as Om Next AST fragments) in the following form:

       {:query <original Om Next query without links>
        :links {[] [<link 1> <link 2>]
                [:foo :bar] [<link 3> <link 4>]}}."
  [query]
  (letfn [(ast-link? [{:keys [key]}]
            (and (vector? key)
                 (= 2 (count key))
                 (or (number? (second key))
                     (string? (second key))
                     (keyword? (second key))
                     (= '_ (second key)))))
          (extract-links* [ast path links]
            (cond-> ast
              (:children ast)
              (update :children
                      (fn [children]
                        (let [child-links (filter ast-link? children)]
                          (when-not (empty? child-links)
                            (swap! links update path
                                   (comp set concat)
                                   child-links)))
                        (into []
                              (comp (remove ast-link?)
                                    (map (fn [{:keys [key] :as child}]
                                           (extract-links* child
                                                           (conj path
                                                                 key)
                                                           links))))
                              children)))))]
    (let [ast               (om-parser/query->ast query)
          links             (atom {})
          ast-without-links (extract-links* ast [] links)]
      {:query (om-parser/ast->expr ast-without-links)
       :links @links})))

(defn query-links
  "Queries all links individually and returns a mapping of query
   paths to link results, each of which is represented as a map
   storing the original link in :link and its query result in
   :result."
  [conn env links]
  (letfn [(query-link [{:keys [key query] :as link}]
            {:link   link
             :result ((:query-link env) env key query)})]
    (into {}
          (map (fn [[path links]]
                 [path (mapv query-link links)]))
          links)))

(defn merge-link-results
  "Merges the results from link queries into a a DataScript query
   result."
  [query-result link-results]
  (letfn [(merge-link-results-at-path [entity-or-entities [path results]]
            (if (bm/collection? entity-or-entities)
              (into #{}
                    (map #(merge-link-results-at-path
                           % [path results]))
                    entity-or-entities)
              (if (empty? path)
                (reduce (fn [entity {:keys [link result]}]
                          (assoc entity (:dispatch-key link) result))
                        entity-or-entities
                        results)
                (update entity-or-entities
                        (first path)
                        merge-link-results-at-path
                        [(rest path) results]))))]
    (reduce merge-link-results-at-path
            query-result
            link-results)))

(defn query-store
  "Executes an Om Next query against DataScript."
  [conn env query clauses]
  (let [{:keys [query links]} (extract-links query)
        ds-query              (datascript-query env query clauses)
        ds-result             (d/q ds-query @conn)
        link-results          (query-links conn env links)
        result                (merge-link-results ds-result
                                                  link-results)]
    result))
