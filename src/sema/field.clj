(ns sema.field)

;; Field state atom with default values
(def field-state (atom {:valence :neutral
                        :density 0.5
                        :decay :medium
                        :history []}))

(defn reset-field!
  "Reset field to default state"
  []
  (reset! field-state {:valence :neutral
                       :density 0.5
                       :decay :medium
                       :history []}))

(defn update-field!
  "Update a field attribute using the provided function"
  [k f]
  (swap! field-state update k f))

(defn set-field!
  "Set a field attribute to a specific value"
  [k v]
  (swap! field-state assoc k v))

(defn add-tension!
  "Add a tension event to the field history with timestamp"
  [tension-map]
  (let [timestamped-tension (assoc tension-map :timestamp (java.util.Date.))]
    (swap! field-state update :history conj timestamped-tension)
    timestamped-tension)) 