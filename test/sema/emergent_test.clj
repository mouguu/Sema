(ns sema.emergent-test
  (:require [clojure.test :refer :all]
            [sema.emergent :as emergent]
            [clara.rules :as clara]))

(deftest test-trigger-individuation
  (testing "individuation triggers when conditions are met"
    (let [session (-> (clara/mk-session 'sema.emergent)
                      (clara/insert (emergent/->Field :curiosity 0.7 :slow))
                      (clara/insert (emergent/->Entity "e1" "semiotic" :dormant))
                      (clara/fire-rules))
          events (clara/query session emergent/get-events)]
      (is (= 1 (count events)))
      (let [event (first events)]
        (is (= :individuation (:type (:?event event))))
        (is (= "e1" (:entity-id (:?event event)))))))
  
  (testing "individuation doesn't trigger when density is too low"
    (let [session (-> (clara/mk-session 'sema.emergent)
                      (clara/insert (emergent/->Field :curiosity 0.5 :slow)) ; Below threshold
                      (clara/insert (emergent/->Entity "e1" "semiotic" :dormant))
                      (clara/fire-rules))
          events (clara/query session emergent/get-events)]
      (is (empty? events))))
  
  (testing "individuation doesn't trigger for non-semiotic substrate"
    (let [session (-> (clara/mk-session 'sema.emergent)
                      (clara/insert (emergent/->Field :curiosity 0.7 :slow))
                      (clara/insert (emergent/->Entity "e1" "material" :dormant)) ; Wrong substrate
                      (clara/fire-rules))
          events (clara/query session emergent/get-events)]
      (is (empty? events))))
  
  (testing "individuation doesn't trigger for non-dormant entities"
    (let [session (-> (clara/mk-session 'sema.emergent)
                      (clara/insert (emergent/->Field :curiosity 0.7 :slow))
                      (clara/insert (emergent/->Entity "e1" "semiotic" :active)) ; Not dormant
                      (clara/fire-rules))
          events (clara/query session emergent/get-events)]
      (is (empty? events))))) 