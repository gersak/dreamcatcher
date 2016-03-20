(ns dreamcatcher.core-test
  (:use clojure.test
        midje.sweet
        dreamcatcher.core))


(println "starting tests")

(letfn
  [(path-history [x] (-> x (update :data conj (state? x))))]
  (defstm simple-stm
    ;; Transitions
    [1 2 path-history
     2 3 path-history
     1 3 path-history
     3 4 path-history
     :any 5 path-history]
    ;; Validators
    [1 3 (fn [x] false)]))


;; Testing transitions
(deftest transition-testing
  (let [i0 (make-stm-instance simple-stm 1 [])
        i2 (-> i0 (move 2))
        i3 (-> i0 (move 2) (move 3))
        i5 (-> i0 (move 5))
        i-full (-> i0 (move 2) (move 3) (move 4) (move 5))]
    (testing "Testing transitions"
      (is (and
            (= (:data i2) [1])
            (= (state? i2) 2)))
      (is (and
            (= (:data i3) [1 2])
            (= (state? i3) 3)))
      (is (= i0 (-> i0 (move 3))))
      (is (and
            (= (:data i5) [1])
            (= (state? i5) 5)))
      (is (and
            (= (:data i-full) [1 2 3 4])
            (= (state? i5) 5))))))

;; Testing


