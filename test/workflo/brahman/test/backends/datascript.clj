(ns workflo.brahman.test.backends.datascript
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [datascript.core :as d]
            [workflo.brahman.backends.datascript :as bds]
            [workflo.brahman.model :as bm]))

(def ^:const +models+
  {:user {:name          :user
          :schema        {:name   [:string :indexed]
                          :email  [:string :indexed]
                          :friend {[:ref :many] '...}
                          :post   {[:ref :many] :post}}
          :stores        [:datascript]
          :derived-attrs [{:name  :friend-count
                           :store :datascript
                           :query '[:find (count ?f) .
                                    :in $ ?n
                                    :where [?u :user/name ?n]
                                           [?u :user/friend ?f]]}]}
   :post {:name          :post
          :schema        {:author {[:ref :one] :user}
                          :title  [:string :indexed]}
          :stores        [:datascript]}})

(deftest fetch-one-query-for-a-model-is-correct
  (let [modeler (bm/modeler {:models (vals +models+)
                             :model->attrs bds/model->attrs
                             :model->joins bds/model->joins})
        model   (bm/get-model modeler :user)]
    (is (= '[:find (pull ?e [:user/name :user/email]) .
             :where [?e :user/name]
                    [?e :db/id 10]]
           (bds/datascript-query {:model      model
                                  :fetch-one? true}
                                 [:user/name :user/email]
                                 '[[?e :db/id 10]])))))

(deftest fetch-one-query-for-another-model-is-correct
  (let [modeler (bm/modeler {:models (vals +models+)
                             :model->attrs bds/model->attrs
                             :model->joins bds/model->joins})
        model   (bm/get-model modeler :post)]
    (is (= '[:find (pull ?e [:post/title]) .
             :where [?e :post/title]
                    [?e :post/title "Hello"]]
           (bds/datascript-query {:model      model
                                  :fetch-one? true}
                                 [:post/title]
                                 '[[?e :post/title "Hello"]])))))

(deftest fetch-many-query-for-a-model-is-correct
  (let [modeler (bm/modeler {:models (vals +models+)
                             :model->attrs bds/model->attrs
                             :model->joins bds/model->joins})
        model   (bm/get-model modeler :user)]
    (is (= '[:find [(pull ?e [:user/name :user/email]) ...]
             :where [?e :user/name]]
           (bds/datascript-query {:model      model
                                  :fetch-one? false}
                                 [:user/name :user/email])))))

(deftest fetch-many-query-for-another-model-is-correct
  (let [modeler (bm/modeler {:models (vals +models+)
                             :model->attrs bds/model->attrs
                             :model->joins bds/model->joins})
        model   (bm/get-model modeler :post)]
    (is (= '[:find [(pull ?e [:post/title]) ...]
             :where [?e :post/title]]
           (bds/datascript-query {:model      model
                                  :fetch-one? false}
                                 [:post/title])))))

(defn install-schemas
  [conn schemas]
  (->> schemas
       (bds/install-schemas [:datascript])
       (reset! conn)))

(defn query-derived-attr
  [conn env attr query entity]
  (bds/query-derived-attr @conn env attr query entity))

(defn query-link
  [conn env [link-name id] query]
  (case link-name
    :link-one (cond-> {:foo "Foo" :bar "Bar"}
                query (select-keys query))
    :link-two (* id 10)))

(defn query-store
  [conn env query params]
  (bds/query-store @conn env query params))

(defn add-entities
  [conn entities]
  (d/transact! conn entities))

(deftest fetching-one-entity-from-datascript-works
  (let [conn    (atom nil)
        modeler (bm/modeler
                 {:models             (vals +models+)
                  :model->attrs       bds/model->attrs
                  :model->joins       bds/model->joins
                  :entity-id          :user/name
                  :install-schemas    (partial install-schemas conn)
                  :query-derived-attr (partial query-derived-attr conn)
                  :query-store        (partial query-store conn)})
        users   (bm/get-model modeler :user)]
    (add-entities @conn
                  [{:db/id       -1
                    :user/name   "Jeff"
                    :user/email  "jeff@jeff.org"
                    :user/friend [-2 -3]
                    :user/post   -4}
                   {:db/id       -2
                    :user/name   "Linda"
                    :user/email  "linda@linda.org"
                    :user/friend -1
                    :user/post   -5}
                   {:db/id       -3
                    :user/name   "Joe"
                    :user/email  "joe@joe.org"}
                   {:db/id       -4
                    :post/title  "Jeff's post"
                    :post/author -1}
                   {:db/id       -5
                    :post/title  "Linda's post"
                    :post/author -2}])
    (and (is (= {:user/name         "Jeff"
                 :user/email        "jeff@jeff.org"
                 :user/friend-count 2}
                (bm/query users [:user/name
                                 :user/email
                                 :user/friend-count]
                          {:fetch-one? true}
                          '[[?e :user/name "Jeff"]])))
         (is (= {:user/email        "linda@linda.org"
                 :user/post         [{:post/title "Linda's post"}]
                 :user/friend       [{:user/name "Jeff"}]
                 :user/friend-count nil}
                (bm/query users
                          [:user/email
                           :user/friend-count
                           {:user/post [:post/title]}
                           {:user/friend [:user/name]}]
                          {:fetch-one? true}
                          '[[?e :user/name "Linda"]]))))))

(deftest fetching-many-entities-from-datascript-works
  (let [conn    (atom nil)
        modeler (bm/modeler
                 {:models             (vals +models+)
                  :model->attrs       bds/model->attrs
                  :model->joins       bds/model->joins
                  :entity-id          :user/name
                  :install-schemas    (partial install-schemas conn)
                  :query-derived-attr (partial query-derived-attr conn)
                  :query-store        (partial query-store conn)})
        users   (bm/get-model modeler :user)
        posts   (bm/get-model modeler :post)]
    (add-entities @conn
                  [{:db/id       -1
                    :user/name   "Jeff"
                    :user/email  "jeff@jeff.org"
                    :user/friend -2
                    :user/post   -3}
                   {:db/id       -2
                    :user/name   "Linda"
                    :user/email  "linda@linda.org"
                    :user/friend -1
                    :user/post   -4}
                   {:db/id       -3
                    :post/title  "Jeff's post"
                    :post/author -1}
                   {:db/id       -4
                    :post/title  "Linda's post"
                    :post/author -2}])
    (and (is (= #{{:user/name "Jeff" :user/email "jeff@jeff.org"}
                  {:user/name "Linda" :user/email "linda@linda.org"}}
                (bm/query users [:user/name :user/email])))
         (is (= #{{:post/title "Jeff's post"
                   :post/author {:user/name "Jeff"}}}
                (bm/query posts
                          [:post/title
                           {:post/author [:user/name]}]
                          {}
                          '[[?e :post/author ?u]
                            [?u :user/name "Jeff"]]))))))

(deftest queries-with-links-work
  (let [conn    (atom nil)
        modeler (bm/modeler
                 {:models             (vals +models+)
                  :model->attrs       bds/model->attrs
                  :model->joins       bds/model->joins
                  :entity-id          :user/name
                  ;; :model->links       bds/model->links
                  :install-schemas    (partial install-schemas conn)
                  :query-derived-attr (partial query-derived-attr conn)
                  :query-link         (partial query-link conn)
                  :query-store        (partial query-store conn)})
        users   (bm/get-model modeler :user)]
    (add-entities @conn
                  [{:db/id       -1
                    :user/name   "Jeff"
                    :user/email  "jeff@jeff.org"
                    :user/friend -2
                    :user/post   -3}
                   {:db/id       -2
                    :user/name   "Linda"
                    :user/email  "linda@linda.org"
                    :user/friend -1
                    :user/post   -4}
                   {:db/id       -3
                    :post/title  "Jeff's post"
                    :post/author -1}
                   {:db/id       -4
                    :post/title  "Linda's post"
                    :post/author -2}])
    (and (is (= {:user/name   "Jeff"
                 :link-one {:foo "Foo" :bar "Bar"}}
                (bm/query users
                          [:user/name [:link-one '_]]
                          {:fetch-one? true}
                          '[[?e :user/name "Jeff"]])))
         (is (= #{{:user/name "Jeff"
                   :link-one {:foo "Foo"}}
                  {:user/name "Linda"
                   :link-one {:foo "Foo"}}}
                (bm/query users
                          [:user/name {[:link-one '_] [:foo]}])))
         (is (= {:user/name   "Jeff"
                 :link-two 100}
                (bm/query users
                          [:user/name [:link-two 10]]
                          {:fetch-one? true}
                          '[[?e :user/name "Jeff"]]))))))
