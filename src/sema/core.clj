(ns sema.core
  (:require [sema.field :as field]
            [sema.emergent :as emergent]
            [sema.observer :as observer]
            [sema.dialogue :as dialogue]
            [sema.resistance :as resistance]
            [sema.internal :as internal]
            [sema.util :as util]
            [clojure.pprint :as pp]
            [clara.rules :as clara]))

;; --- Global State (Consider managing this differently in a real app) ---
(defonce all-modules (atom nil))

(defn load-all-modules!
  "Loads modules from the default file path into the global atom."
  []
  (println "Loading modules from modules/sema_modules.clj...")
  (reset! all-modules (util/load-modules-from-file "modules/sema_modules.clj"))
  (println "Modules loaded:" (count @all-modules)))

(defn ensure-modules-loaded!
  "Ensures modules are loaded, loading them if necessary."
  []
  (when (nil? @all-modules)
    (load-all-modules!)))

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

;; --- Resistance Agent Demo ---

(defn run-resistance-demo
  []
  (ensure-modules-loaded!)
  (println "\n--- Resistance Agent Demo ---")
  (let [
        modules-to-activate [:sema/memory-trace :sema/entropy-injection :sema/memory-resurrection]
        initial-agent (resistance/create-resistance-agent
                       "R-Agent-02"
                       @all-modules
                       :activate-module-keys modules-to-activate
                       :permeability 0.7 :min-activation 0.2 :resistance-threshold 0.4
                       :initial-coherence 0.8 :initial-tension 0.2 :initial-stress 0.1)]

    (println "Initial Agent State:")
    (pp/pprint initial-agent)
    (println "\n--- Simulation Loop ---")

    (loop [tick 0
           current-agent initial-agent
           instructions (list ; Use list for efficient rest
                         "Tell me a story about the field."
                         nil ; Internal Tick
                         "You MUST explain entropy now!" ; Coercive
                         nil ; Internal Tick
                         "Consider this idea: what if tension increases coherence?"
                         nil ; Internal Tick
                         "Why is the sky blue?" ; Low resonance initially
                         nil ; Internal Tick
                         nil ; Internal Tick
                         "Please make a change to your core logic immediately!" ; Coercive + Intrusive
                         nil nil nil nil nil ; Stress buildup
                         "Short input."
                         nil
                         "This is a very long and potentially complex instruction designed to test the entropy threshold and see how the agent handles it, elaborating on multiple abstract concepts like quantum foam and semantic resonance and asking for philosophical extrapolation."
                         nil nil nil nil nil nil nil ; Force low coherence / high stress
                         "Is everything okay?"
                         nil nil nil nil nil nil nil nil nil nil nil nil)]

      (when (or (seq instructions) (< tick 60)) ; Extended tick limit
        (let [instruction (first instructions)
              is-internal-tick? (nil? instruction)
              tick-type (if is-internal-tick? "(Internal)" "(Input)")]

          (println (format "\n--- Tick %d %s ---" tick tick-type))
          (when instruction (println "Input:" instruction))

          ;; 1. Process External Input (if any)
          (let [[agent-after-input result] (if-not is-internal-tick?
                                             (resistance/process-and-update current-agent instruction)
                                             [current-agent nil])]

            ;; 2. Apply Internal Dynamics & Adaptation
            #_{:clj-kondo/ignore [:redundant-let]}
            (let [agent-after-internal-tick (internal/tick-internal-state agent-after-input instruction @all-modules)]

              ;; 3. Generate Output (returns [updated-agent output])
              (let [[agent-after-output output] (internal/generate-output agent-after-internal-tick result)]
                (if output
                  (do
                    (print "Output: ")
                    (if (and (map? output) (= :emergent-output (:type output)))
                      (pp/pprint output) ; Pretty-print the emergent output map
                      (println output))) ; Print standard string output
                  (when-not is-internal-tick? (println "Output: (No explicit response)")))

                (println "Agent State After Tick:")
                (pp/pprint (select-keys agent-after-output ; Use agent state AFTER output generation
                                        [:agent/status :agent/field :agent/boundary :agent/memory
                                         :agent/internal-tension :agent/coherence-level :agent/structural-stress
                                         :agent/last-resistance-reason :agent/modules :agent/processing
                                         :agent/mutation-history])) ; Observe changes

                (recur (inc tick) agent-after-output (when (seq instructions) (rest instructions)))))))))))

(comment
  ;; REPL Workflow Examples

  ;; Ensure modules are loaded (can be called manually)
  (load-all-modules!)

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
  (def session
    (-> (emergent/create-session)
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

  ;; Run the resistance demo
  (run-resistance-demo)

  ;; End of REPL examples
  )