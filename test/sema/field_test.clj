(ns sema.field-test
  (:require [clojure.test :refer :all]
            [sema.field :as field]))

(deftest test-update-field
  (testing "updating field values"
    (field/reset-field!) ; Reset to initial state
    (is (= 0.5 (get @field/field-state :density)))
    (field/update-field! :density inc)
    (is (= 0.6 (get @field/field-state :density)))
    (field/update-field! :density #(- % 0.2))
    (is (= 0.4 (get @field/field-state :density)))))

(deftest test-add-tension
  (testing "adding tension updates history"
    (field/reset-field!) ; Reset to initial state
    (is (empty? (get @field/field-state :history)))
    (field/add-tension! {:type :curiosity :magnitude 0.3})
    (is (= 1 (count (get @field/field-state :history))))
    (let [tension (first (get @field/field-state :history))]
      (is (= :curiosity (:type tension)))
      (is (= 0.3 (:magnitude tension)))
      (is (instance? java.util.Date (:timestamp tension)))))) 