(ns sema.adaptation)

;; --- Adaptation Strategy ---
(defn- calculate-adaptation-strategy
  "Determines an overall adaptation strategy based on state zones."
  [agent]
  (let [coherence (get agent :agent/coherence-level 0.5)
        tension   (get agent :agent/internal-tension 0.5)
        stress    (get agent :agent/structural-stress 0.5)]
    (cond
      ;; Zone: Collapse Imminent (Very low coherence, very high stress/tension)
      (and (< coherence 0.15) (> stress 0.85) (> tension 0.8)) :emergency-stabilize
      ;; Zone: High Strain (Low coherence or High stress/tension)
      (or (< coherence 0.3) (> stress 0.7) (> tension 0.8)) :reduce-strain
      ;; Zone: Stagnation (High coherence, low stress/tension)
      (and (> coherence 0.9) (< stress 0.1) (< tension 0.2)) :explore-perturb
      ;; Zone: Stable
      :else :maintain-stability)))

;; --- Parameter Adaptation Logic (Strategy-Based) ---
(defn adapt-parameters
  "Adapts agent parameters based on sustained states and adaptation strategy."
  [agent]
  (let [strategy (calculate-adaptation-strategy agent)
        coherence (get agent :agent/coherence-level 0.5)
        tension (get agent :agent/internal-tension 0.5)
        stress (get agent :agent/structural-stress 0.5)
        ticks-low-coherence (get-in agent [:agent/counters :low-coherence] 0)
        ticks-high-tension (get-in agent [:agent/counters :high-tension] 0)
        ticks-high-stress (get-in agent [:agent/counters :high-stress] 0)]
    (println " Adaptation Strategy:" strategy)

    (let [;; Apply general counter resets first
          agent-with-reset-counters (cond-> agent 
                                      (and (< coherence 0.8) (< ticks-low-coherence 2)) (assoc-in [:agent/counters :low-coherence] 0)
                                      (and (< tension 0.7) (< ticks-high-tension 3)) (assoc-in [:agent/counters :high-tension] 0)
                                      (and (< stress 0.6) (< ticks-high-stress 2)) (assoc-in [:agent/counters :high-stress] 0))] 
      (case strategy
        :emergency-stabilize
        (cond-> agent-with-reset-counters
          true (update-in [:agent/boundary :permeability] #(* % 0.5))
          true (update-in [:agent/field :resistance-threshold] #(min 1.0 (+ % 0.1)))
          (get-in agent [:agent/modules :sema/entropy-injection])
          (assoc-in [:agent/modules :sema/entropy-injection :activation-threshold] 0.1))

        :reduce-strain
        (cond-> agent-with-reset-counters
          (> ticks-low-coherence 5)
          (-> (update-in [:agent/boundary :permeability] #(min 1.0 (+ % 0.01)))
              (update-in [:agent/field :resistance-threshold] #(max 0.0 (- % 0.005))))
          (> ticks-high-tension 8)
          (update-in [:agent/field :resistance-threshold] #(min 1.0 (+ % 0.01)))
          (> ticks-high-stress 6)
          (update-in [:agent/boundary :permeability] #(max 0.05 (- % 0.02)))
          (and (> ticks-high-stress 4) (> stress 0.7) (get-in agent [:agent/modules :sema/entropy-injection]))
          (update-in [:agent/modules :sema/entropy-injection :activation-threshold] #(min 0.9 (+ % 0.03))))

        :explore-perturb
        (cond-> agent-with-reset-counters
          (> coherence 0.95)
          (-> (update-in [:agent/boundary :permeability] #(min 0.9 (+ % 0.02)))
              (update-in [:agent/field :resistance-threshold] #(max 0.1 (- % 0.01))))
          (get-in agent [:agent/modules :sema/entropy-injection])
          (update-in [:agent/modules :sema/entropy-injection :activation-threshold] #(max 0.2 (- % 0.05))))
        
        :maintain-stability
        agent-with-reset-counters ; No significant changes

        ;; Default case
        agent-with-reset-counters))))

;; --- Module Reconfiguration Logic (Strategy-Based) ---
(defn adapt-active-modules
  "Dynamically activates/deactivates modules based on strategy and state."
  [agent all-available-modules]
  (let [strategy (calculate-adaptation-strategy agent)
        active-modules (:agent/modules agent)
        has-module? (fn [k] (contains? all-available-modules k))
        is-active? (fn [k] (contains? active-modules k))] 
    (case strategy
      :emergency-stabilize
      (cond-> agent
        (and (has-module? :sema/memory-resurrection) (not (is-active? :sema/memory-resurrection)))
        (assoc-in [:agent/modules :sema/memory-resurrection] (get all-available-modules :sema/memory-resurrection))
        (is-active? :sema/entropy-injection)
        (update :agent/modules dissoc :sema/entropy-injection))

      :reduce-strain
      (cond-> agent
        (and (> (get agent :agent/structural-stress) 0.75) (has-module? :sema/contradiction-handler) (not (is-active? :sema/contradiction-handler)))
        (assoc-in [:agent/modules :sema/contradiction-handler] (get all-available-modules :sema/contradiction-handler))
        (and (< (get agent :agent/coherence-level) 0.25) (has-module? :sema/memory-resurrection) (not (is-active? :sema/memory-resurrection)))
        (assoc-in [:agent/modules :sema/memory-resurrection] (get all-available-modules :sema/memory-resurrection))
        (and (< (get agent :agent/structural-stress) 0.3) (is-active? :sema/contradiction-handler))
        (update :agent/modules dissoc :sema/contradiction-handler))

      :explore-perturb
      (cond-> agent
        (and (has-module? :sema/entropy-injection) (not (is-active? :sema/entropy-injection)))
        (assoc-in [:agent/modules :sema/entropy-injection] (get all-available-modules :sema/entropy-injection))
        (is-active? :sema/memory-resurrection)
        (update :agent/modules dissoc :sema/memory-resurrection))

      :maintain-stability
       (cond-> agent
          (and (> (get agent :agent/coherence-level) 0.9) (< (get agent :agent/tension) 0.1) (is-active? :sema/entropy-injection))
          (update :agent/modules dissoc :sema/entropy-injection))

      agent))) ; Default case

;; --- Processing Pipeline Mutation Logic ---
(defn- should-mutate-pipeline?
  "Decides if the pipeline should mutate based on extreme states."
  [agent]
  (and (> (get agent :agent/structural-stress 0) 0.9) ; High stress
       (< (get agent :agent/coherence-level 1) 0.15) ; Low coherence
       (> (rand) 0.7))) ; Probabilistic trigger

(defn mutate-processing-pipeline
  "Applies a random mutation to the agent's processing pipeline."
  [agent]
  (if (should-mutate-pipeline? agent)
    (let [current-pipeline (get agent :agent/processing [])
          available-steps [:signal-nullifier :instruction-resistor :boundary-regulator
                           :semantic-quarantine :structural-integrity-check :resonance-check]
          mutation-type (rand-nth [:reorder :remove :duplicate :add])]
      (println "** Pipeline Mutation Triggered! Type:" mutation-type " **")
      (case mutation-type
        :reorder (assoc agent :agent/processing (shuffle current-pipeline))
        :remove (if (> (count current-pipeline) 2) 
                  (assoc agent :agent/processing (vec (take (max 1 (rand-int (dec (count current-pipeline)))) (shuffle current-pipeline))))
                  agent) 
        :duplicate (if (and (not-empty current-pipeline) (< (count current-pipeline) 8)) 
                     (let [step-to-dup (rand-nth current-pipeline)]
                       (assoc agent :agent/processing (conj current-pipeline step-to-dup)))
                     agent) 
        :add (if (< (count current-pipeline) 8)
                (let [step-to-add (rand-nth available-steps)]
                  (assoc agent :agent/processing (conj current-pipeline step-to-add)))
                agent) 
        agent)) 
    agent)) 