(ns sema.observer
  (:require [clara.rules :as clara]
            [sema.emergent :as emergent]
            [clojure.pprint :as pp]))

(defn observe-state
  "Query and format the state of a Clara session for human consumption"
  [session]
  (let [fields (clara/query session emergent/get-fields)
        entities (clara/query session emergent/get-entities)
        events (clara/query session emergent/get-events)]
    (with-out-str
      (println "=== Observed State ===")
      (println "\nFields:")
      (doseq [field fields]
        (pp/pprint (:?field field)))
      
      (println "\nEntities:")
      (doseq [entity entities]
        (pp/pprint (:?entity entity)))
      
      (println "\nEvents:")
      (doseq [event events]
        (pp/pprint (:?event event)))
      
      (println "\n====================="))))

(defn observe-field-state
  "Get a formatted string representation of the current field state"
  [field-state]
  (with-out-str
    (println "=== Field State ===")
    (pp/pprint (dissoc field-state :history))
    (println "\nHistory (last 5 entries):")
    (doseq [entry (take 5 (reverse (:history field-state)))]
      (pp/pprint entry))
    (println "=================="))) 