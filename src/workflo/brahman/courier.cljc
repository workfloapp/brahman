(ns workflo.brahman.courier
  #?(:clj (:import [clojure.lang ExceptionInfo]))
  (:refer-clojure :exclude [deliver])
  (:require [workflo.brahman.model :as bm]
   #?(:cljs [cljs.core :refer [ExceptionInfo]])))

;;;; Courier protocol

(defprotocol ICourier
  (dispatch [this cmd] [this cmd env])
  (get-model [this model-name]))

;;;; Command validation

(defmulti validate-rule (fn [_ _ _ _ rule] (:type rule)))

(defmethod validate-rule :model
  [courier cmd spec env {:keys [select model]}]
  (let [params   (second cmd)
        selector (cond->> select (not (vector? select)) (vector))
        param    (get-in params selector)
        model    (get-model courier model)
        errors   (bm/validate model param)]
    (when errors
      (throw
       (ex-info (str "Command '" (first cmd) "' is invalid")
                {:cmd cmd
                 :validation-errors errors})))))

(defmethod validate-rule :default
  [courier cmd spec env rule]
  (throw
   (ex-info "Validation rule '" rule "' unknown")))

(defn validate [courier cmd spec env]
  (let [rules (:validations spec)]
    (every? (fn [rule]
              (validate-rule courier cmd spec env rule))
            rules)))

;;;; Command authorization

(defmulti authorize-rule* (fn [_ _ _ _ rule] (:type rule)))

(defmethod authorize-rule* :env
  [courier cmd spec env rule]
  {:pre [(contains? rule :select-env)
         (contains? rule :test)]}
  (let [env-select   (:select-env rule)
        env-selector (cond-> env-select
                       (not (vector? env-select)) vector)
        env-value    (get-in env env-selector)
        test-fn      (:test rule)]
    (if (cond
          (contains? rule :test-param)
          (let [params         (second cmd)
                param-select   (:test-param rule)
                param-selector (cond-> param-select
                                 (not (vector? param-select)) vector)
                param-value    (get-in params param-selector)]
            (test-fn env-value param-value))

          (contains? rule :test-value)
          (test-fn env-value (:test-value rule))

          :else
          (test-fn env-value))
      true
      (throw
       (ex-info (str "Command '" (first cmd) "' failed to authorize")
                {:cmd cmd})))))

(declare authorize-rule)

(defn authorize-or-rules
  [courier cmd spec env rules]
  (letfn [(authorize-or-rule [res rule]
            (try
              (do
                (authorize-rule courier cmd spec env rule)
                (assoc res :one-ok? true))
              (catch ExceptionInfo e
                (update res :errors conj e))))]
    (let [res (reduce authorize-or-rule
                      {:errors [] :one-ok? false}
                      rules)]
      (if (:one-ok? res)
        true
        (throw (first (:errors res)))))))

(defn authorize-rule
  [courier cmd spec env rule]
  (if (set? rule)
    (authorize-or-rules courier cmd spec env rule)
    (authorize-rule* courier cmd spec env rule)))

(defn authorize [courier cmd spec env]
  (let [rules (:authorizations spec)]
    (every? #(authorize-rule courier cmd spec env %) rules)))

;;;; Command delivery

(defn deliver [courier cmd spec env]
  (let [config (:config courier)]
    ((:deliver config) courier cmd env)))

;;;; Courier implementation

(defrecord Courier [config]
  ICourier
  (dispatch [this cmd]
    (dispatch this cmd {}))

  (dispatch [this cmd env]
    {:pre [(list? cmd)
           (symbol? (first cmd))
           (or (= 1 (count cmd))
               (map? (second cmd)))]}
    (let [name (first cmd)
          spec (get-in config [:commands name])]
      (if spec
        (do
          (validate this cmd spec env)
          (authorize this cmd spec env)
          (deliver this cmd spec env))
        (throw
         (ex-info (str "The command '" name "' is unknown")
                  {:cmd cmd})))))

  (get-model [this model]
    (let [config (:config this)]
      ((:get-model config) this model))))

(defn courier
  [{:keys [commands
           deliver
           get-model]
    :or {commands  []
         deliver   #()
         get-model #()}}]
  (let [config   {:commands  commands
                  :deliver   deliver
                  :get-model get-model}
        courier' (Courier. config)]
    courier'))
