(ns sema.util
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn load-modules-from-file
  "Reads module definitions from an EDN file.
   Expects a file containing a sequence of vectors, where each vector is
   [:module/keyword {:config map}].
   Returns a map of {:module/keyword {:config map}}."
  [filepath]
  (try
    (with-open [rdr (io/reader filepath)]
      (let [modules-vec (edn/read (java.io.PushbackReader. rdr))]
        (if (and (sequential? modules-vec) (every? vector? modules-vec))
          (into {} (map (fn [[k v]] {k v}) modules-vec))
          (do
            (println "Warning: Could not parse modules file correctly." filepath)
            {}))))
    (catch java.io.FileNotFoundException _
      (println "Warning: Modules file not found:" filepath)
      {})
    (catch Exception e
      (println "Error reading modules file:" filepath "-" (.getMessage e))
      {})))