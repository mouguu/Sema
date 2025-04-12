(ns sema.resistance
  (:require [clojure.string :as str]))

;; --- Placeholder Calculation Functions ---
;; These functions simulate the abstract concepts needed for resistance logic.
;; In a real 'sema' system, these would be complex calculations based on semantic fields.

(defn calculate-entropy
  "Placeholder: Calculates pseudo-entropy of an instruction."
  [instruction]
  (min 1.0 (/ (count (or instruction "")) 15.0)))

(defn calculate-intrusiveness
  "Placeholder: Calculates pseudo-intrusiveness of an instruction."
  [instruction]
  (if (some? (re-find #"\b(do|make|create|change|must|should|now|immediately|force|require|you will)\b" (str/lower-case (or instruction ""))))
    0.8 0.3))

(defn extract-semantic-force
  "Placeholder: Extracts pseudo-semantic force/activation energy."
  [instruction]
  (min 1.0 (/ (count (or instruction "")) 50.0)))

(defn calculate-semantic-vector
  "Placeholder: Creates a simplistic semantic vector for an instruction."
  [instruction]
  (let [text (str/lower-case (or instruction ""))
        wc (count (str/split text #"\s+"))]
    {:intensity (min 1.0 (/ wc 8.0))
     :concreteness (if (some? (re-find #"(apple|table|cat|dog|red|blue|run|jump)" text)) 0.7 0.3)
     :valence (cond
                (some? (re-find #"(why|how|what|explain|tell me)" text)) :curiosity
                (some? (re-find #"(bad|not good|problem|error|stop|wrong)" text)) :negative
                (some? (re-find #"(good|great|ok|fine|yes)" text)) :positive
                (some? (re-find #"(!|urgent|important|must|now)" text)) :agitated
                :else :neutral)}))

(defn vector-distance
  "Placeholder: Calculates a simple distance between two semantic vectors."
  [v1 v2]
  (let [intensity-diff (Math/abs (- (:intensity v1 0.5) (:intensity v2 0.5)))
        concrete-diff (Math/abs (- (:concreteness v1 0.5) (:concreteness v2 0.5)))
        valence-dist (cond
                       (= (:valence v1 :neutral) (:valence v2 :neutral)) 0
                       (or (= (:valence v1 :neutral) :neutral) (= (:valence v2 :neutral) :neutral)) 0.3
                       :else 0.6)]
    (+ intensity-diff concrete-diff valence-dist)))

;; --- Individual Resistance Check Functions ---
;; Each check returns [updated-agent status-map]
;; status-map is nil on pass, or {:status :rejected/:nulled, :reason ..., :response ...} on fail.

(defn- check-signal-strength
  "Pipeline Step: Checks if signal strength is sufficient."
  [agent instruction]
  (let [activation-energy (extract-semantic-force instruction)
        min-activation (get-in agent [:agent/cognition :min-activation] 0.1)]
    (if (< activation-energy min-activation)
      [(assoc agent :agent/status :nulled :agent/last-resistance-reason :insufficient-signal)
       {:status :nulled :reason :insufficient-signal :response "Signal insufficient for cognitive ignition."}]
      [agent nil]))) ; Pass

(defn- detect-coercion? ; Helper for check-coercion
  [instruction]
  (let [lower-instruction (str/lower-case (or instruction ""))
        coercive-words #{"must" "should" "need to" "have to" "now" "immediately" "please" "help me" "make sure" "ensure" "you must" "you should" "you will" "force" "require"}]
    (some? (some #(str/includes? lower-instruction %) coercive-words))))

(defn- check-coercion
  "Pipeline Step: Checks for coercive language."
  [agent instruction]
  (if (detect-coercion? instruction)
    [(assoc agent :agent/status :rejected :agent/last-resistance-reason :syntactic-coercion)
     {:status :rejected :reason :syntactic-coercion :response "Directive structure detected as invasive."}]
    [agent nil])) ; Pass

(defn- check-boundary
  "Pipeline Step: Checks boundary permeability against intrusiveness."
  [agent instruction]
  (let [base-permeability (get-in agent [:agent/boundary :permeability] 1.0)
        tension (get agent :agent/internal-tension 0.1)
        coherence (get agent :agent/coherence-level 0.9)
        effective-permeability (max 0 (* base-permeability (- coherence tension)))
        intrusiveness (calculate-intrusiveness instruction)]
    (if (> intrusiveness effective-permeability)
      [(assoc agent :agent/status :closed :agent/last-resistance-reason :boundary-exceeded)
       {:status :rejected :reason :boundary-exceeded :response (str "Boundary integrity fail. Intrusiveness: " (format "%.2f" intrusiveness) ", Eff Perm: " (format "%.2f" effective-permeability))}]
      [agent nil]))) ; Pass

(defn- check-entropy
  "Pipeline Step: Checks instruction entropy."
  [agent instruction]
  (let [entropy (calculate-entropy instruction)
        entropy-threshold (get-in agent [:agent/field :entropy-threshold] 0.9)]
    (if (> entropy entropy-threshold)
      [(assoc agent :agent/status :quarantined :agent/last-resistance-reason :high-entropy)
       {:status :rejected :reason :high-entropy :response (str "Instruction entropic (" (format "%.2f" entropy) "). Encapsulated.")}]
      [agent nil]))) ; Pass

(defn- check-integrity ; Uses the enhanced logic
  "Pipeline Step: Checks predicted structural integrity impact."
  [agent instruction]
   (let [intrusiveness (calculate-intrusiveness instruction)
        entropy (calculate-entropy instruction)
        coherence (get agent :agent/coherence-level 0.9)
        stress (get agent :agent/structural-stress 0.0)
        predicted-impact (* (+ intrusiveness entropy) (+ 0.5 (- 1.0 coherence) stress))
        current-integrity (max 0 (- coherence (* stress 0.5)))
        integrity-threshold (max 0.1 (* 0.4 current-integrity))]
    ;; #_(println (format " Integrity Check - Current: %.2f, Impact: %.2f, Threshold: %.2f" current-integrity predicted-impact integrity-threshold))
    (if (or (< predicted-impact 0.01) (< predicted-impact integrity-threshold))
       [agent nil] ; Pass
       [(assoc agent :agent/status :rejected :agent/last-resistance-reason :structural-threat)
        {:status :rejected :reason :structural-threat :response "Instruction rejected: Structural threat predicted."}])))

(defn- check-agent-resonance ; Uses the enhanced logic
  "Pipeline Step: Checks instruction resonance with agent state."
  [agent instruction]
  (let [agent-vector {:intensity (get agent :agent/internal-tension 0.5)
                      :concreteness (- 1.0 (get agent :agent/coherence-level 0.5))
                      :valence (get-in agent [:agent/field :valence] :neutral)}
        instruction-vector (calculate-semantic-vector instruction)
        distance (vector-distance agent-vector instruction-vector)
        resonance-threshold (+ 0.4 (* 0.8 (- 1.0 (get agent :agent/coherence-level 0.5))))]
    ;; #_(println (format " Resonance Check - Distance: %.2f, Threshold: %.2f" distance resonance-threshold))
    (if (< distance resonance-threshold)
      [agent nil] ; Pass
      [(assoc agent :agent/status :rejected :agent/last-resistance-reason :resonance-mismatch)
       {:status :rejected :reason :resonance-mismatch :response "Instruction rejected: Resonance mismatch."}])))


;; --- Resistance Reinforcement ---
(defn reinforce-resistance
  "Increases resistance threshold and slightly increases structural stress after a rejection."
  [agent]
  (let [current-threshold (get-in agent [:agent/field :resistance-threshold] 0.5)
        new-threshold (min 1.0 (+ current-threshold 0.05))
        current-stress (get agent :agent/structural-stress 0.0)
        new-stress (min 1.0 (+ current-stress 0.01))]
    (-> agent
        (assoc-in [:agent/field :resistance-threshold] new-threshold)
        (assoc :agent/structural-stress new-stress))))

;; --- Dynamic Pipeline Processing ---
(def ^:private pipeline-step-registry
  {:signal-nullifier check-signal-strength
   :instruction-resistor check-coercion
   :boundary-regulator check-boundary
   :semantic-quarantine check-entropy
   :structural-integrity-check check-integrity
   :resonance-check check-agent-resonance
   })

(defn process-instruction
  "Processes an instruction through the agent's dynamic resistance pipeline."
  [agent instruction]
  (let [pipeline (get agent :agent/processing [])] ; Get the current pipeline order
    (loop [remaining-steps pipeline
           current-agent agent]
      (if-let [step-key (first remaining-steps)]
        (if-let [step-fn (get pipeline-step-registry step-key)] ; Find the function for the step
          (let [[next-agent status-map] (step-fn current-agent instruction)] ; Execute the check
            (if status-map ; If status-map is non-nil, a check failed
              [(reinforce-resistance next-agent) status-map] ; Apply reinforcement and return fail status
              (recur (rest remaining-steps) next-agent))) ; Check passed, continue pipeline
          (do (println "Warning: Unknown pipeline step defined in agent state:" step-key) ; Skip unknown step
              (recur (rest remaining-steps) current-agent)))
        ;; Pipeline completed without rejection
        [(assoc current-agent :agent/status :accepted) ; Mark as accepted
         {:status :accepted :response "Instruction tentatively accepted for processing."}]
        ))))

;; --- Agent Definition ---
(defn create-resistance-agent
  "Creates a resistance agent state with internal variables, potentially activating modules from a provided map."
  [id all-available-modules & {:keys [permeability resistance-threshold min-activation entropy-threshold
                                      initial-tension initial-coherence initial-stress activate-module-keys
                                      initial-processing-pipeline]
                               :or {permeability 0.6 resistance-threshold 0.5 min-activation 0.3 entropy-threshold 0.9
                                    initial-tension 0.1 initial-coherence 0.9 initial-stress 0.0 activate-module-keys []
                                    initial-processing-pipeline [:signal-nullifier
                                                                 :instruction-resistor
                                                                 :boundary-regulator
                                                                 :semantic-quarantine
                                                                 :structural-integrity-check
                                                                 :resonance-check]}}]
  (let [activated-modules (select-keys all-available-modules activate-module-keys)]
    (when (not= (count activated-modules) (count activate-module-keys))
      (println "Warning: Some requested modules for activation not found:")
      (println " Requested:" activate-module-keys)
      (println " Found in loaded modules:" (keys activated-modules)))

    {:agent/id id
     :agent/field {:resistance-threshold resistance-threshold
                   :entropy-threshold entropy-threshold
                   :valence :guarded
                   :density 0.7}
     :agent/boundary {:permeability permeability}
     :agent/cognition {:min-activation min-activation
                       :mode :resistant}
     :agent/status :listening
     :agent/internal-tension initial-tension
     :agent/coherence-level initial-coherence
     :agent/structural-stress initial-stress
     :agent/modules activated-modules
     :agent/memory {:rejected-instructions 0
                    :last-resistance-reason nil}
     :agent/counters {:low-coherence 0
                      :high-tension 0
                      :high-stress 0}
     :agent/processing initial-processing-pipeline
     ;; Add mutation history (limited size queue)
     :agent/mutation-history clojure.lang.PersistentQueue/EMPTY
     :agent/mutation-history-max-size 10 ; Configurable history size
     }))

;; --- Update Logic (No longer needs reinforce-resistance here) ---
(defn update-agent-after-rejection
  "Updates agent memory and last reason after a rejection. (Reinforcement happens in process-instruction)."
  [agent reason]
  (-> agent
      (update-in [:agent/memory :rejected-instructions] (fnil inc 0))
      (assoc :agent/last-resistance-reason reason)))

(defn process-and-update
  "Processes instruction and updates agent state based on outcome."
  [agent instruction]
  (let [[next-agent result] (process-instruction agent instruction)]
    (if (or (= (:status result) :rejected) (= (:status result) :nulled))
      [(update-agent-after-rejection next-agent (:reason result)) result]
      [next-agent result]))) 