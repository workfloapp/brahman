#!/usr/bin/env boot

(set-env!
 :source-paths #{"test"}
 :resource-paths #{"resources" "src"}
 :dependencies '[;; Boot
                 [adzerk/boot-test "1.0.6"]

                 ;; Language libraries
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]

                 ;; Datomic
                 [com.datomic/datomic-free "0.9.5344"]
                 [datomic-schema "1.3.0"]

                 ;; Others
                 [bouncer "1.0.0"]
                 [com.rpl/specter "0.9.1"]

                 ;; Tests
                 [org.clojure/test.check "0.9.0"]])

(task-options!
 pom {:project 'brahman
      :version "0.1.0-SNAPSHOT"})

(require '[adzerk.boot-test :refer [test]])

(deftask test-auto
  []
  (comp (watch)
        (test)))

(deftask install-local
  []
  (comp
   (pom)
   (jar)
   (install)))
