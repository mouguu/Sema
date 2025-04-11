(ns sema.emergent
  (:require [clara.rules :refer [defrule defquery insert! mk-session]]
            [clara.rules.accumulators :as acc]))

;; Define record structures for facts
(defrecord Field [valence density decay])
(defrecord Entity [id substrate state])
(defrecord Event [type entity-id timestamp details])

;; Define queries to extract information from the session
(defquery get-fields
  []
  [?field <- Field])

(defquery get-entities
  []
  [?entity <- Entity])

(defquery get-events
  []
  [?event <- Event])

;; Define rule for individuation process
(defrule trigger-individuation
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