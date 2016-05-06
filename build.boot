#!/usr/bin/env boot

(set-env!
 :source-paths #{"test"}
 :resource-paths #{"resources" "src"}
 :dependencies '[;; Boot
                 [adzerk/boot-test "1.0.6" :scope "test"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]

                 ;; Language libraries
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]

                 ;; Datomic
                 [com.datomic/datomic-free "0.9.5344"]
                 [datomic-schema "1.3.0"]

                 ;; Others
                 [bouncer "1.0.0"]
                 [com.rpl/specter "0.9.1"]
                 [org.omcljs/om "1.0.0-alpha32"]

                 ;; Tests
                 [org.clojure/test.check "0.9.0"]])

(require '[adzerk.boot-test :refer [test]]
         '[adzerk.bootlaces :refer :all]
         '[boot.git :refer [last-commit]])

(def +project+ 'workflo/brahman)
(def +version+ "0.3.0-SNAPSHOT")

(bootlaces! +version+ :dont-modify-paths? true)

(task-options!
 pom  {:project        +project+
       :version        +version+
       :description    (str "A universe of backends for Om Next & "
                            "other CQRS systems")
       :url            "https://github.com/workfloapp/brahman"
       :scm            {:url "https://github.com/workfloapp/brahman"}
       :license        {"GNU Lesser General Public License 2.1"
                        "http://www.gnu.org/licenses/lgpl-2.1.html"}}
 push {:repo           "deploy-clojars"
       :ensure-branch  "master"
       :ensure-clean   true
       :ensure-tag     (last-commit)
       :ensure-version +version+})

(deftask test-auto
  []
  (comp (watch)
        (test)))

(deftask install-local
  []
  (comp (pom)
        (jar)
        (install)))

(deftask deploy-snapshot
  []
  (comp (pom)
        (jar)
        (build-jar)
        (push-snapshot)))

(deftask deploy-release
  []
  (comp (pom)
        (jar)
        (build-jar)
        (push-release)))
