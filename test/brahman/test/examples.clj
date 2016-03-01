(ns brahman.test.examples
  (:import [clojure.lang ExceptionInfo])
  (:require [bouncer.core :as b]
            [bouncer.validators :as v]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is use-fixtures]]
            [com.rpl.specter :as s]
            [brahman.authnz :as ba]
            [brahman.courier :as bc]
            [brahman.model :as bm]
            [brahman.backends.datomic :as bd]
            [brahman.test.setup :as setup :refer [create-db
                                                  delete-db
                                                  with-conn
                                                  connect-to-db]]))

;;;; Datomic setup

(defn datomic-fixture [f]
  (delete-db)
  (create-db)
  (f)
  (delete-db))

(use-fixtures :each datomic-fixture)

;;;; Model configuration

(defn install-schemas [schemas]
  (doseq [[store models] schemas]
    (case store
      :datomic (bd/install-schemas (connect-to-db) models))))

(defn query-source
  [env query extra])

;;;; Courier configuration

(defn deliver-command [courier cmd env]
  (println "DELIVER COMMAND" cmd env)
  true)

;;;; Tests

(deftest user-model-and-commands
  (let [;; Define a simple user model
        user-model {:name       :user
                    :version    1
                    :schema     {:username [:string :indexed]
                                 :email    [:string :indexed]
                                 :name     [:string]
                                 :role     [:enum [:regular :admin]]
                                 :friend   {[:ref :many] '...}}
                    :validation {:user/username [v/required v/string]
                                 :user/email    [v/required v/email]
                                 :user/name     [v/string]}
                    :sources
                    [;; Fetch users from a Datomic db
                     {:type      :store
                      :store     :datomic
                      :query     '[:find [(pull ?user ATTRS) ...]
                                   :in $ ATTRS
                                   :where [?user :user/name _]]}
                     ;; Derive a :user/popular? attribute for every user
                     ;; that is computed when running queries against the
                     ;; model
                     {:type      :derived-attr
                      :attr      :popular?
                      :store     :datomic
                      :query     '[:find (count ?friend) .
                                   :in $ ?user
                                   :where [?user :user/friend ?friend]]
                      :transform [[s/ALL] (fn [friends]
                                            (and (not (nil? friends))
                                                 (pos? friends)))]}]}
        modeler    (bm/modeler {:models          [user-model]
                                :install-schemas install-schemas
                                :schema->attrs   bd/schema->attrs
                                :validate        (fn [model rules data]
                                                   (-> data
                                                       (b/validate rules)
                                                       (first)))
                                :entity-id       :db/id
                                :query-source    query-source})
        commands   {'user/create
                    {:version        1
                     :authorizations []
                     :validations    [;; Validate using the user model
                                      {:type       :model
                                       :select     :user
                                       :model      :user}]}
                    'user/update
                    {:version        1
                     :validations    [;; Validate using the user model
                                      {:type       :model
                                       :select     :user
                                       :model      :user}]
                     :authorizations [;; Is the user executing the command
                                      ;; authenticated?
                                      {:type       :env
                                       :select-env :auth-user
                                       :test       ba/set?}
                                      ;; Is the user executing the command
                                      ;; either an admin or the same as the
                                      ;; user being modified?
                                      #{{:type       :env
                                         :select-env [:auth-user
                                                      :user/username]
                                         :test       ba/equals?
                                         :test-param [:user
                                                      :user/username]}
                                        {:type       :env
                                         :select-env [:auth-user
                                                      :user/role]
                                         :test       ba/equals?
                                         :test-value :admin}}]}}
        courier    (bc/courier {:commands  commands
                                :deliver   deliver-command
                                :get-model (fn [courier model]
                                             (bm/get-model modeler
                                                           model))})]
    ;; Assert there are no users in the beginning
    (let [users (bm/query (bm/get-model modeler :user) nil '[*])]
      (is (and (set? users) (empty? users))))

    ;; Dispatch an invalid create command with no user
    (let [cmd '(user/create)]
      (is (thrown? ExceptionInfo (bc/dispatch courier cmd))))

    ;; Dispatch an invalid create command with an incomplete user
    (let [cmd '(user/create {:user {:user/name "John Doe"}})]
      (is (thrown? ExceptionInfo (bc/dispatch courier cmd))))

    ;; Dispatch a valid create command
    (let [cmd '(user/create {:user {:user/name "Jeff Doe"
                                    :user/email "jeff@doe.name"
                                    :user/username "jeff"}})]
      (is (bc/dispatch courier cmd)))

    ;; Assert there is a single user "jeff" now
    (let [users (bm/query (bm/get-model modeler :user) nil '[*])]
      (and (is (set? users))
           (is (= 1 (count users)))
           (is (= {:user/name "Jeff Doe"
                   :user/email "jeff@doe.name"
                   :user/username "jeff"}
                  (select-keys (first users) [:user/name
                                              :user/email
                                              :user/username])))))

    ;; Dispatch an invalid update command with no user
    (let [cmd '(user/update)]
      (is (thrown? ExceptionInfo (bc/dispatch courier cmd))))

    ;; Dispatch an invalid update command with an incomplete user
    (let [cmd '(user/update {:user {:user/name "John Doe"}})]
      (is (thrown? ExceptionInfo (bc/dispatch courier cmd))))

    ;; Dispatch a valid update command with a missing auth user
    (let [cmd '(user/update {:user {:user/name "Jeff Doe"
                                    :user/email "jeff@doe.name"
                                    :user/username "jeff"}})
          env {}]
      (is (thrown? ExceptionInfo (bc/dispatch courier cmd env))))

    ;; Dispatch a valid update command with the wrong auth user
    (let [cmd '(user/update {:user {:user/name "Jeff Doe"
                                    :user/email "jeff@doe.name"
                                    :user/username "jeff"}})
          env {:auth-user {:user/username "joe"}}]
      (is (thrown? ExceptionInfo (bc/dispatch courier cmd env))))

    ;; Dispatch a valid update command with the auth user being
    ;; the same as the user being updated
    (let [cmd '(user/update {:user {:user/name "Jeff Doe"
                                    :user/email "jeff@doe.name"
                                    :user/username "jeff"}})
          env {:auth-user {:user/username "jeff"}}]
      (is (bc/dispatch courier cmd env)))

    ;; Dispatch a valid update command with the auth user being
    ;; a different user - but an admin
    (let [cmd '(user/update {:user {:user/name "Jeff Doe"
                                    :user/email "jeff@doe.name"
                                    :user/username "jeff"}})
          env {:auth-user {:user/username "joe"
                           :user/role :admin}}]
      (is (bc/dispatch courier cmd env)))))
