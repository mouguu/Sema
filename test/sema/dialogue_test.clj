(ns sema.dialogue-test
  (:require [clojure.test :refer [deftest is testing]]
            [sema.dialogue :as dialogue]
            [sema.field :as field]
            [clojure.string :as str]))

(deftest test-dialogue-step
  (testing "dialogue-step with update density"
    (field/reset-field!)
    (let [response (dialogue/dialogue-step "update density to 0.8")]
      (is (string? response))
      (is (str/includes? response "density"))
      (is (= 0.8 (get @field/field-state :density)))))
  
  (testing "dialogue-step with add tension"
    (field/reset-field!)
    (let [response (dialogue/dialogue-step "add tension of type curiosity with magnitude 0.4")]
      (is (string? response))
      (is (str/includes? response "tension"))
      (is (= 1 (count (get @field/field-state :history))))
      (is (= :curiosity (:type (first (get @field/field-state :history)))))
      (is (= 0.4 (:magnitude (first (get @field/field-state :history)))))))
  
  (testing "dialogue-step with observe"
    (field/reset-field!)
    (let [response (dialogue/dialogue-step "observe the current state")]
      (is (string? response))
      (is (str/includes? response "Field state"))
      (is (str/includes? response "density"))))) 