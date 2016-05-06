(ns workflo.brahman.test.backends.datascript
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [datascript.core :as d]
            [workflo.brahman.backends.datascript :as bds]
            [workflo.brahman.model :as bm]))

(def ^:const +models+
  {:user {:name   :user
          :schema {:name   [:string :indexed]
                   :email  [:string :indexed]
                   :friend {[:ref :many] '...}
                   :post   {[:ref :many] :post}}
          :stores [:datascript]}
   :post {:name   :post
          :schema {:author {[:ref :one] :user}
                   :title  [:string :indexed]}
          :stores [:datascript]}})

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
                  :install-schemas    (partial install-schemas conn)
                  :query-derived-attr (partial query-derived-attr conn)
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
    (and (is (= {:user/name "Jeff" :user/email "jeff@jeff.org"}
                (bm/query users [:user/name :user/email]
                          {:fetch-one? true}
                          '[[?e :user/name "Jeff"]])))
         (is (= {:user/email "linda@linda.org"
                 :user/post [{:post/title "Linda's post"}]
                 :user/friend [{:user/name "Jeff"}]}
                (bm/query users
                          [:user/email
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
