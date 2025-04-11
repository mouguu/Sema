(ns sema.dialogue
  (:require [sema.field :as field]
            [sema.observer :as observer]
            [clojure.string :as str]))

(defn extract-number
  "Extract the first number from a string"
  [s]
  (let [matcher (re-matcher #"(\d+\.\d+|\d+)" s)
        found (re-find matcher)]
    (when found
      (read-string (second found)))))

(defn handle-update-density
  "Handle a request to update the density field"
  [intent]
  (if-let [new-density (extract-number intent)]
    (do
      (field/set-field! :density new-density)
      (format "Field density updated to %.2f" new-density))
    "Could not extract a valid density value from the intent"))

(defn handle-add-tension
  "Handle a request to add tension to the field"
  [intent]
  (let [type-match (re-find #"type\s+(\w+)" intent)
        mag-match (re-find #"magnitude\s+(\d+\.\d+|\d+)" intent)
        tension-type (when type-match (keyword (second type-match)))
        magnitude (when mag-match (read-string (second mag-match)))]
    (if (and tension-type magnitude)
      (do
        (field/add-tension! {:type tension-type :magnitude magnitude})
        (format "Added tension of type %s with magnitude %.2f" 
                (name tension-type) magnitude))
      "Could not extract valid tension parameters from the intent")))

(defn handle-observe-state
  "Handle a request to observe the current field state"
  [_]
  (observer/observe-field-state @field/field-state))

(defn dialogue-step
  "Process a user intent string and respond with appropriate action"
  [user-intent-string]
  (let [intent (str/lower-case user-intent-string)]
    (cond
      (str/includes? intent "update density") 
      (handle-update-density intent)
      
      (str/includes? intent "add tension") 
      (handle-add-tension intent)
      
      (str/includes? intent "observe") 
      (handle-observe-state intent)
      
      :else 
      "I don't understand that intent. Try 'update density', 'add tension', or 'observe'."))) 