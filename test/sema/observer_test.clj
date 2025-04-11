(ns sema.observer-test
  (:require [clojure.test :refer [deftest is testing]]
            [sema.observer :as observer]
            [sema.emergent :as emergent]
            [clara.rules :as clara]
            [clojure.string :as str]))

(deftest test-observe-state
  (testing "observe-state should format session facts"
    (let [session (-> (clara/mk-session 'sema.emergent)
                      (clara/insert (emergent/->Field :curiosity 0.7 :slow))
                      (clara/insert (emergent/->Entity "e1" "semiotic" :dormant))
                      (clara/fire-rules))
          observed (observer/observe-state session)]
      (is (string? observed))
      (is (str/includes? observed "Field"))
      (is (str/includes? observed ":curiosity"))
      (is (str/includes? observed "Entity"))
      (is (str/includes? observed "e1"))
      (is (str/includes? observed "Event"))
      (is (str/includes? observed ":individuation"))))) 