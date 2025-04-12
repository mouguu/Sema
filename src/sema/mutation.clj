(ns sema.mutation)

(def ^:private base-symbol-pools
  {:punctuation [".,?!"]
   :operators   ["~-+*/%<>="]
   :abstract    ["^∆∞Σ∫Ωµ"]
   :structure   ["()[]{}" "|"]
   :binary      ["01"]
   :meta        [":;_#@$" "\\" "\\\\"]})

(defn- select-symbol-pool
  "Selects symbol pools dynamically based on agent state (coherence, stress, tension)."
  [agent]
  (let [coherence (get agent :agent/coherence-level 0.5)
        stress    (get agent :agent/structural-stress 0.5)
        tension   (get agent :agent/internal-tension 0.5)
        ;; Determine pool categories based on state
        categories (cond
                     ;; Collapse state: High stress, low coherence -> structural/abstract/operators
                     (and (> stress 0.85) (< coherence 0.2))
                     (do (println " Selecting pools for Collapse State") [:structure :abstract :operators])
                     ;; High stress state -> structure & operators
                     (> stress 0.7)
                     (do (println " Selecting pools for High Stress State") [:structure :operators :punctuation])
                     ;; High tension state -> abstract & operators & punctuation
                     (> tension 0.8)
                     (do (println " Selecting pools for High Tension State") [:abstract :operators :punctuation :binary])
                     ;; Low coherence state -> more diverse pools, less structure?
                     (< coherence 0.3)
                     (do (println " Selecting pools for Low Coherence State") (shuffle (keys base-symbol-pools)))
                     ;; Stable state -> Punctuation, operators, maybe some structure/meta
                     :else
                     (do (println " Selecting pools for Stable State") [:punctuation :operators :structure :meta]))
        ;; Select pools based on categories, introduce slight randomness
        selected-pools (->> categories
                            (keep base-symbol-pools) ; Use keep to map and filter nil results
                            (flatten)
                            (shuffle)
                            ;; Slightly bias towards taking more pools if available
                            (take (max 1 (+ 1 (rand-int (inc (count categories))))))
                            )]
    (if (empty? selected-pools)
      (do (println " WARN: No pools selected based on state, using random fallback.")
          (rand-nth (vals base-symbol-pools))) ; Fallback: pick pools from a random category
      selected-pools)))

(defn- calculate-mutation-params
  "Calculates length and complexity parameters based on agent state, using dynamic pools."
  [agent]
  (let [stress (get agent :agent/structural-stress 0.0)
        coherence (get agent :agent/coherence-level 0.5)
        tension (get agent :agent/internal-tension 0.5)
        ;; Length now influenced by coherence too (less coherent -> potentially shorter bursts?)
        base-length (+ 1 (int (* 8 (+ stress tension) (- 1.0 coherence)))) 
        length-variation (* base-length 0.6 (- (rand) 0.5))
        max-length (max 1 (min 15 (int (+ base-length length-variation)))) ; Limit max length
        ;; Dynamically select pools based on full state
        pools (select-symbol-pool agent)
        available-symbols (apply str pools)]
     (println (str " Mutation Params - Length: " max-length ", Symbols from pools: " (pr-str pools)))
    {:max-length max-length
     :symbols available-symbols}))

;; --- History & Mutation Logic (Includes Self-Assessment Concept) ---

#_{:clj-kondo/ignore [:unused-binding]}
(defn- update-mutation-history
  "Adds a new mutation record and potentially assesses effectiveness of the *previous* one."
  [agent new-mutation-record previous-state]
  (let [history (get agent :agent/mutation-history clojure.lang.PersistentQueue/EMPTY)
        max-size (get agent :agent/mutation-history-max-size 10)
        current-stress (get agent :agent/structural-stress)
        current-tension (get agent :agent/internal-tension)
        
        ;; Assess the effectiveness of the *last* mutation in the history (if any)
        history-with-assessment (if-let [last-mutation (peek history)] ; Get the most recent entry without removing
                                  (let [prev-stress (get-in last-mutation [:generating-state :agent/structural-stress])
                                        prev-tension (get-in last-mutation [:generating-state :agent/internal-tension])
                                        stress-delta (- current-stress prev-stress)
                                        tension-delta (- current-tension prev-tension)
                                        ;; Simple effectiveness: significant reduction in stress/tension is good
                                        effectiveness-boost (cond 
                                                             (and (< stress-delta -0.1) (< tension-delta -0.1)) 0.3 ; Big improvement
                                                             (or (< stress-delta -0.05) (< tension-delta -0.05)) 0.1 ; Some improvement
                                                             :else -0.05) ; No improvement or worse -> slight penalty
                                        current-effectiveness (get last-mutation :effectiveness 0.3) ; Start with a base effectiveness
                                        new-effectiveness (max 0 (min 1 (+ current-effectiveness effectiveness-boost)))
                                        ;; Update the last element in the queue (needs vec conversion)
                                        history-vec (vec history)
                                        updated-last-mutation (assoc last-mutation :effectiveness new-effectiveness)]
                                     (println (format "Assessed last mutation: Stress Δ%.2f, Tension Δ%.2f -> Eff: %.2f" stress-delta tension-delta new-effectiveness))
                                     (conj (vec (pop history-vec)) updated-last-mutation)) ; Replace last element
                                  history) ; No history to assess
        
        ;; Add the new record (without effectiveness score initially)
        history-with-new (conj history-with-assessment (assoc new-mutation-record :effectiveness 0.3))] ; New records start average

    ;; Ensure history does not exceed max size
    (assoc agent :agent/mutation-history (if (> (count history-with-new) max-size)
                                             (pop history-with-new) ; Remove oldest if exceeds max size
                                             history-with-new))))

(defn- get-historical-protocol
  "Attempts to retrieve a protocol from history, biased towards more effective ones."
  [agent]
  (let [history (vec (get agent :agent/mutation-history clojure.lang.PersistentQueue/EMPTY))]
    (when (and (seq history) (> (rand) 0.4)) ; 60% chance to use history if available
      ;; Weighted random selection based on effectiveness
      (let [total-effectiveness (max 0.01 (reduce + (map :effectiveness history))) ; Avoid division by zero
            rand-val (* (rand) total-effectiveness)]
        (loop [remaining-history history
               cumulative-eff 0]
          (if-let [mutation (first remaining-history)]
            (let [eff (get mutation :effectiveness 0.01) ; Min effectiveness > 0
                  next-cumulative (+ cumulative-eff eff)]
              (if (> next-cumulative rand-val)
                (:output mutation) ; Select this one
                (recur (rest remaining-history) next-cumulative)))
            ;; Fallback if loop finishes (shouldn't happen with max 0.01)
            (:output (rand-nth history))))))))


(defn- mutate-protocol
  "Applies a simple mutation (char swap, add, delete) to a protocol string."
  [protocol symbols]
   (if (or (empty? protocol) (empty? symbols)) ; Check symbols too
    protocol
    (let [len (count protocol)
          mutation-type (rand-nth [:swap :add :delete :noop])]; Added noop
      (case mutation-type
        :swap (let [idx1 (rand-int len) idx2 (rand-int len)]
                (-> (vec protocol) (assoc idx1 (get protocol idx2)) (assoc idx2 (get protocol idx1)) (->> (apply str))))
        :add (let [idx (rand-int (inc len)) char-to-add (rand-nth symbols)]
               (str (subs protocol 0 idx) char-to-add (subs protocol idx)))
        :delete (if (> len 1) (let [idx (rand-int len)] (str (subs protocol 0 idx) (subs protocol (inc idx)))) protocol) 
        :noop protocol ; No change
        protocol))))

(defn generate-emergent-protocol
  "Generates a non-standard output string, potentially reusing/mutating history (biased by effectiveness)."
  [agent]
  (let [{:keys [max-length symbols]} (calculate-mutation-params agent)]
    (if (empty? symbols)
      "[silence: no symbols available]"
      (if-let [historical-proto (get-historical-protocol agent)]
        (if (> (rand) 0.3) ; 70% chance to mutate history, 30% to reuse directly
          (mutate-protocol historical-proto symbols)
          (do (println " Reusing historical protocol directly.") historical-proto))
        ;; No history used, generate fresh protocol
        (apply str (repeatedly max-length #(rand-nth symbols)))))))


(defn invoke-emergent-output-generator
  "Generates a structured emergent output map and updates history (with assessment)."
  [agent previous-tick-state] ; Needs previous state for assessment
  (let [protocol (generate-emergent-protocol agent)
        semantic-tag (cond
                       (< (:agent/coherence-level agent) 0.2) :structure-decay
                       (> (:agent/structural-stress agent) 0.8) :stress-burst
                       (> (:agent/internal-tension agent) 0.9) :tension-overflow
                       :else :state-anomaly)
        mutation-record {:type :emergent-output
                         :output protocol
                         :tag semantic-tag
                         :generating-state (select-keys agent [:agent/coherence-level
                                                               :agent/structural-stress
                                                               :agent/internal-tension])
                         ;; Effectiveness will be added when the *next* mutation occurs
                         :timestamp (java.util.Date.)} 
        ;; Pass current agent state (before adding new record) to assess the *previous* mutation
        agent-with-history (update-mutation-history agent mutation-record previous-tick-state)] 
    [agent-with-history mutation-record]))