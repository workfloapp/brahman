# Introducing Brahman

_A universe of backends for Om Next & other CQRS systems._

## What is Brahman and what can it do for me?

Brahman aims to solve the problem of connecting client-driven CQRS
systems like Om Next with server side databases, event sourcing and
business logic. It offers a mostly declarative way of writing
colocated specifications for data models, data validation, commands,
command validation and command authorization, and a simple interface
to query/process these models and commands.

### Motivation

With Om Next, most of the client side complexity of querying and
mutating data goes away. However, there is no equivalent story for an
equally tidy, declarative, colocated, extensible way of handling
queries and mutations on the server side. Brahman aims to provide one.

We realized that there are all these nice libraries and tools out there
for writing backends - Sente, Buddy / Friend, Datomic, Bouncer /
Prismatic Schema, Onyx, Kafka to name just a few - but integrating these
with Om Next queries and mutations is not straight forward enough.

Also, even though Datomic performs data-level validation and Buddy /
Friend provide general authnz functionality, our feeling was that almost
everyone will be looking for a way to perform validation that yields
human-readable errors and authorization that operates on commands
rather than e.g. HTTP requests, typically from within an Om Next server
side parser or similar.

Which leads us to Brahman's features:

### Features

* Models
  - Define models as materialized views on top of _any_ number
    of databases
  - Fetch and query model data using _any_ query language
  - Specify model version (in any format), schemas (in any format),
    validation rules (in any format) and data stores (of any kind)
    all in one place (colocation), in a declarative way, as data
  - Extend data with attributes derived from any of the databases
    on the fly
* Commands
  - Define commands in the system, including their versions, validation
    and authorization rules, in one place (colocation), declaratively,
    as data
  - Validate commands and command parameters
      * Using model schemas and validations
      * Using database queries
      * Using arbitrary functions / predicates
  - Authorize commands
      * Using the command environment (can include arbitrary information
        about client, authenticated user, database etc.)
      * Using database queries
      * Using arbitrary functions / predicates
      * Producing validation and authorization errors that are readable
        for humans and machines at the same time
  - Verify that all commands used in the system are validated
    and authorized

## How does it work?

### 1. Define data models

```
;; Define a user model (using datomic-schema in this case)
(def user-model
  {:name       :user
   :version    1
   :schema     {:username   [:string :indexed]
                :email      [:string :indexed]
                :name       [:string]
                :friend     {[:ref :many] '...}}
   :validation {:username   [v/required v/string]
                :email      [v/required v/email]
                :name       [v/string]
                :friend     [v/every '...]}
   :sources    [{:name      :db
                 :source    :datomic
                 :type      :datomic
                 :query     '[:find (pull ?user ATTRS)
                              :in $ ATTRS
                              :where [?user :user/username _]]}
                {:name      :popular?
                 :type      :derived-attr
                 :query     '[:find (count ?friend) .
                              :in $ ?user
                              :where [?user :user/friend ?friend]]
                 :transform [[s/ALL] (fn [friends]
                                       (and (not (nil? friends))
                                            (pos? friends)))]}]})

;; Define commands to operate on users
(def commands
  {'user/create
   {:version        1
    :authorizations []
    :validations    [{:type         :model
                      :param        :user
                      :model        :user}]}
   'user/update
   {:version        1
    :authorizations [{:type         :env
                      :select-env   :auth-user
                      :test         #'brahman.authnz/set?}
                     {:type         :env
                      :select-env   :auth-user
                      :test         #'brahman.authnz/equals-param?
                      :compare-param [:user :username]}
                     ;; What about checks like:
                     ;;
                     ;; Is this an admin user?
                     ;;   -> {:type          :env
                     ;;       :select-env    [:auth-user :user/role?]
                     ;;       :test          #'brahman.authnz/equals?
                     ;;       :test-value    :roles/admin}
                     ;;
                     ;; Has the auth user created this user?
                     ;;   -> {:type          :env
                     ;;       :select-query  <DataScript query with the env map as its input>
                     ;;       :test          #'brahman.authnz/equals?
                     ;;       :test-value    ...}
                     ;;
                     ;; Is this user updating himself OR is he an admin?
                     ;; Idea: Treat vectors of rules like `and` and sets of rules as `or`?
                     ;;   -> #{<rule 1> <rule2>}
                     ;;
                     ;; Arbitrary function:
                     ;;   -> {:type :pred
                     ;;       :pred (fn [env cmd] ...)}
                     ]
    :validations    [{:type         :model
                      :param        :user
                      :model        :user}]}})
```
