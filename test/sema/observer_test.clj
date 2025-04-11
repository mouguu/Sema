(ns sema.observer-test
  (:require [clojure.test :refer :all]
            [sema.observer :as observer]
            [sema.emergent :as emergent]
            [clara.rules :as clara]))

(deftest test-observe-state
  (testing "observe-state should format session facts"
    (let [session (-> (clara/mk-session 'sema.emergent)
                      (clara/insert (emergent/->Field :curiosity 0.7 :slow))
                      (clara/insert (emergent/->Entity "e1" "semiotic" :dormant))
                      (clara/fire-rules))
          observed (observer/observe-state session)]
      (is (string? observed))
      (is (clojure.string/includes? observed "Field"))
      (is (clojure.string/includes? observed ":curiosity"))
      (is (clojure.string/includes? observed "Entity"))
      (is (clojure.string/includes? observed "e1"))
      (is (clojure.string/includes? observed "Event"))
      (is (clojure.string/includes? observed ":individuation"))))) 