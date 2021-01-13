(ns lazy-filter-test
    (:use lazy-filter)
    (:require [clojure.test :as test]))
    
(test/deftest base_workflow 
    (test/testing "Parallel-filter mimics normal filter"
        (test/is (= (take 200 (filter (fn [x] (= 0 (mod x 3))) (range))) (take 200 (lazy-filter (fn [x] (= 0 (mod x 3))) (range) 4 5))))))
    
(test/run-tests 'lazy-filter-test)
