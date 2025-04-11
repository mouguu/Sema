(ns sema.core
  (:require [sema.field :as field]
            [sema.emergent :as emergent]
            [sema.observer :as observer]
            [sema.dialogue :as dialogue]
            [clara.rules :as clara]))

(defn create-example-session
  "Create a session with example entities for demonstration"
  []
  (-> (emergent/create-session)
      (clara/insert (emergent/->Field :curiosity 0.5 :slow))
      (clara/insert (emergent/->Entity "e1" "semiotic" :dormant))
      (clara/insert (emergent/->Entity "e2" "material" :dormant))
      (clara/fire-rules)))

(defn run-example
  "Run a complete example of the sema simulation"
  []
  (println "Initializing sema simulation...\n")
  
  ;; Reset field state
  (field/reset-field!)
  (println "Field initialized:")
  (println (observer/observe-field-state @field/field-state))
  
  ;; Create session with entities
  (let [session (create-example-session)]
    (println "Initial session state:")
    (println (observer/observe-state session))
    
    ;; Update field density to trigger individuation
    (println "\nUpdating field density to 0.7...")
    (field/set-field! :density 0.7)
    
    ;; Add some tension
    (println "Adding tension of type curiosity...")
    (field/add-tension! {:type :curiosity :magnitude 0.6})
    (println (observer/observe-field-state @field/field-state))
    
    ;; Create new session with updated field
    (let [updated-session (-> (emergent/create-session)
                              (clara/insert (emergent/->Field (:valence @field/field-state) 
                                                             (:density @field/field-state)
                                                             (:decay @field/field-state)))
                              (clara/insert (emergent/->Entity "e1" "semiotic" :dormant))
                              (clara/insert (emergent/->Entity "e2" "material" :dormant))
                              (clara/fire-rules))]
      (println "\nUpdated session state (after density increase):")
      (println (observer/observe-state updated-session)))))

(comment
  ;; REPL Workflow Examples
  
  ;; Initialize field
  (field/reset-field!)
  
  ;; Update field properties
  (field/set-field! :valence :curiosity)
  (field/update-field! :density inc)
  
  ;; Add tension
  (field/add-tension! {:type :curiosity :magnitude 0.3})
  
  ;; Observe field state
  (println (observer/observe-field-state @field/field-state))
  
  ;; Create a session
  (def session (-> (emergent/create-session)
                  (clara/insert (emergent/->Field (:valence @field/field-state) 
                                                 (:density @field/field-state)
                                                 (:decay @field/field-state)))
                  (clara/insert (emergent/->Entity "e1" "semiotic" :dormant))
                  (clara/fire-rules)))
  
  ;; Observe session state
  (println (observer/observe-state session))
  
  ;; Use dialogue-step for natural language interaction
  (dialogue/dialogue-step "update density to 0.8")
  (dialogue/dialogue-step "add tension of type curiosity with magnitude 0.5")
  (dialogue/dialogue-step "observe the current state")
  
  ;; Run the full example
  (run-example)
  
  ;; End of REPL examples
  ) 