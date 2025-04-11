(ns sema.emergent
  (:require [clara.rules :refer [defrule defquery insert! mk-session query]]))

;; Define record structures for facts
(defrecord Field [valence density decay])
(defrecord Entity [id substrate state])
(defrecord Event [type entity-id timestamp details])

;; Define queries to extract information from the session
(defquery ^:public get-fields-query
  []
  [?field <- Field])

(defquery ^:public get-entities-query
  []
  [?entity <- Entity])

(defquery ^:public get-events-query
  []
  [?event <- Event])

;; Public wrapper functions for the queries
(defn get-fields
  "Get all Field facts from the session"
  [session]
  (query session get-fields-query))

(defn get-entities
  "Get all Entity facts from the session"
  [session]
  (query session get-entities-query))

(defn get-events
  "Get all Event facts from the session"
  [session]
  (query session get-events-query))

;; This rule will be loaded by Clara when mk-session is called with
;; this namespace. Linters may incorrectly flag it as unused.
;; Clara Rules requires this rule to be defined at the top level.
#_{:clj-kondo/ignore [:unused-private-var]}
(defrule ^:private trigger-individuation
  "Triggers individuation process when field density exceeds threshold and 
   a dormant semiotic entity exists"
  [Field (> density 0.6)]
  [?entity <- Entity (= substrate "semiotic") (= state :dormant)]
  =>
  (let [event (->Event :individuation (:id ?entity) (java.util.Date.) 
                      {:description "Entity is individuating due to sufficient field density"})]
    (println "Individuation triggered for entity" (:id ?entity))
    (insert! event)))

(defn create-session
  "Create a new Clara session with the emergent rules"
  []
  (mk-session 'sema.emergent)) 