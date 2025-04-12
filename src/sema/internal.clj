(ns sema.internal
  (:require [sema.resistance :as resistance]
            [sema.mutation :as mutation]
            [sema.adaptation :as adaptation]))

;; --- Placeholder Functions for Complex Concepts ---

(defn calculate-structural-stress
  "Placeholder: Estimate stress. Increased penalty for low coherence."
  [agent]
  (let [rejected-count (get-in agent [:agent/memory :rejected-instructions] 0)
        ;; Increased penalty for low coherence
        low-coherence-penalty (* 0.2 (- 1.0 (get agent :agent/coherence-level 1.0)))]
    (min 1.0 (+ low-coherence-penalty (* rejected-count 0.02)))))

(defn calculate-coherence-change
  "Placeholder: Estimate coherence change. High tension inhibits natural recovery."
  [agent maybe-instruction]
  (let [current-tension (get agent :agent/internal-tension 0.0)
        natural-drift-factor (max 0 (- 1.0 (* 1.5 current-tension))) ; Reduce drift when tension is high
        base-drift (if maybe-instruction
                     (let [entropy (resistance/calculate-entropy maybe-instruction)
                           intrusiveness (resistance/calculate-intrusiveness maybe-instruction)]
                       (- (* (+ entropy intrusiveness) 0.015))) ; Slightly stronger decrease from complex input
                     (* 0.005 natural-drift-factor)) ; Natural drift depends on tension
        stress-impact (* -0.05 (get agent :agent/structural-stress 0))] 
    (+ base-drift stress-impact)))

(defn calculate-tension-change
  "Placeholder: Calculate tension change. Low coherence now slightly increases tension (instability)."
  [agent maybe-instruction]
  (let [input-impact (if maybe-instruction
                       (- (resistance/calculate-intrusiveness maybe-instruction) 0.4)
                       -0.05) 
        stress-factor (* (get agent :agent/structural-stress 0) 0.1)
        ;; Low coherence slightly INCREASES tension (instability)
        coherence-factor (* 0.08 (- 1.0 (get agent :agent/coherence-level 1.0)))] 
    (-> (+ input-impact stress-factor coherence-factor)
        (* 0.1)
        )))

(defn check-module-trigger
  "Checks if conditions are met to trigger a module-driven action, using module config."
  [agent module-key]
  (if-let [module-config (get-in agent [:agent/modules module-key])]
    (case module-key
      :sema/entropy-injection
      (let [tension (get agent :agent/internal-tension 0)
            activation-threshold (get module-config :activation-threshold 0.8)] ; Use threshold from config
        (and (> tension activation-threshold) (> (rand) 0.5)))

      :sema/memory-resurrection ; Example: Trigger if coherence is low
      (let [coherence (get agent :agent/coherence-level 1.0)
            threshold (get module-config :threshold 0.4)] ; Example threshold
        (< coherence threshold))

      ;; Add checks for other modules here based on their config and agent state
      false) ; Default: module not triggerable or check not implemented
    false)) ; Module not active

;; --- Internal State Update Functions ---

(defn update-internal-variable
  "Safely updates an internal variable within bounds [0, 1]."
  [agent key change-fn]
  (let [current-val (get agent key 0.5)
        change (change-fn agent)
        new-val (-> (+ current-val change) (max 0.0) (min 1.0))]
    (assoc agent key new-val)))

(defn update-stress [agent]
  (update-internal-variable agent :agent/structural-stress calculate-structural-stress))

(defn update-coherence [agent maybe-instruction]
  (update-internal-variable agent :agent/coherence-level #(calculate-coherence-change % maybe-instruction)))

(defn update-tension [agent maybe-instruction]
  (update-internal-variable agent :agent/internal-tension #(calculate-tension-change % maybe-instruction)))

(defn update-state-counters
  "Updates counters tracking sustained states."
  [agent]
  (let [coherence (get agent :agent/coherence-level 0.5)
        tension (get agent :agent/internal-tension 0.5)
        stress (get agent :agent/structural-stress 0.5)]
    (-> agent
        (update-in [:agent/counters :low-coherence] #(if (< coherence 0.3) (inc %) 0))
        (update-in [:agent/counters :high-tension] #(if (> tension 0.8) (inc %) 0))
        (update-in [:agent/counters :high-stress] #(if (> stress 0.7) (inc %) 0)))))

;; --- Module Effects ---

(defn apply-module-decay-effects
  "Apply decay effects from active modules (e.g., memory trace)."
  [agent]
  (if-let [config (get-in agent [:agent/modules :sema/memory-trace])]
    (let [decay-factor (case (:decay-function config)
                         :non-linear-entropic 0.02
                         0.01)
          stability (get config :trace-stability 0.5)]
      (update-internal-variable agent :agent/coherence-level #(* decay-factor (- 1.0 stability) -1)))
    agent))

(defn apply-entropy-injection
  "Apply entropy injection effects if module is active and triggered."
  [agent]
  (if (check-module-trigger agent :sema/entropy-injection)
    (let [config (get-in agent [:agent/modules :sema/entropy-injection])
          amplitude (get config :amplitude 0.5)]
      (println "** Module Triggered: Entropy Injection! **")
      (-> agent
          (assoc-in [:agent/field :valence] (rand-nth [:curiosity :neutral :guarded :agitated :confused])) ; Added :confused
          (update-in [:agent/boundary :permeability] #(max 0.1 (min 0.9 (+ % (* amplitude (- (rand) 0.5))))))))
    agent))

(defn apply-memory-resurrection
  "Placeholder: Apply memory resurrection effects (e.g., boost coherence)."
  [agent]
  (if (check-module-trigger agent :sema/memory-resurrection)
    (do
      (println "** Module Triggered: Memory Resurrection Attempt! **")
      (update-internal-variable agent :agent/coherence-level (fn [_] 0.1))) ; Small coherence boost
    agent))

;; --- Internal Tick Orchestration ---

(defn apply-module-effects
  "Sequentially applies effects for all active modules that trigger."
  [agent]
  (-> agent
      (apply-module-decay-effects)
      (apply-entropy-injection)
      (apply-memory-resurrection)
      ))

(defn tick-internal-state
  "Applies state evolution, counters, module effects, and adaptation (including pipeline mutation)."
  [agent maybe-instruction all-available-modules]
  (let [; Store the state *before* this tick's updates for potential mutation assessment later
        previous-tick-state agent 
        agent-after-updates (-> agent
                              (update-stress)
                              (update-coherence maybe-instruction)
                              (update-tension maybe-instruction)
                              (update-state-counters)
                              (apply-module-effects))]

    ;; Apply adaptation logic AFTER primary state updates
    (-> agent-after-updates
        (adaptation/adapt-parameters)
        (adaptation/adapt-active-modules all-available-modules)
        (adaptation/mutate-processing-pipeline)
        ;; Store the state before this tick's updates for the *next* tick's assessment
        (assoc :agent/previous-tick-state previous-tick-state) 
        )))

;; --- Output Generation (Modified) ---

(defn should-trigger-mutation?
  "Check if conditions are met to trigger emergent output generation."
  [agent]
  (or (< (get agent :agent/coherence-level 1.0) 0.15)
      (and (< (get agent :agent/coherence-level 1.0) 0.3)
           (> (get agent :agent/structural-stress 0.0) 0.85))))

(defn should-emit-internal-signal?
  "Decide if the agent should emit a standard internal signal (if mutation not triggered)."
  [agent]
  (and (>= (get agent :agent/coherence-level 1.0) 0.15)
       (or (> (get agent :agent/internal-tension 0) 0.9)
           (> (get agent :agent/structural-stress 0.0) 0.8))))

(defn generate-standard-internal-signal
  "Generate a standard signal based on the agent's internal state."
  [agent]
  (cond
    (> (get agent :agent/internal-tension 0) 0.9)
    (str "[Internal Signal: Field tension critical - " (format "%.2f" (get agent :agent/internal-tension)) "]")
    (> (get agent :agent/structural-stress 0.0) 0.8)
    (str "[Internal Signal: Structural stress high - " (format "%.2f" (get agent :agent/structural-stress)) "]")
    :else
    "[Internal Signal: State nominal]"))

(defn generate-output
  "Generate output: response, mutation, or internal signal. Returns [updated-agent output]."
  [agent process-result]
  (let [previous-state (get agent :agent/previous-tick-state agent)] ; Get previous state, fallback to current
    (cond
      ;; Priority 1: Respond to instruction
      process-result
      [agent (:response process-result)]

      ;; Priority 2: Trigger emergent output, passing previous state for assessment
      (should-trigger-mutation? agent)
      (mutation/invoke-emergent-output-generator agent previous-state)

      ;; Priority 3: Emit standard internal signal
      (should-emit-internal-signal? agent)
      [agent (generate-standard-internal-signal agent)]

      ;; Otherwise, no output
      :else [agent nil]))) 