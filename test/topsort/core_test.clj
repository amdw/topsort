(ns topsort.core-test
  (:require [clojure.test :refer :all]
            [topsort.core :refer :all]
            [clojure.set :as sets]))

(deftest graph-reader-test
  (doseq [[input expected-graph]
          [[["a b c"]              {:a [:b :c]}]
           [["b c" "a b c"]        {:a [:b :c] :b [:c]}]
           [["a b c" "c"]          {:a [:b :c] :c []}]
           [["a b c" "" "c"]       {:a [:b :c] :c []}]
           [["a b c" " b c" " c "] {:a [:b :c] :b [:c] :c []}]]]
    (testing (str "Test valid read for " input)
      (is (= expected-graph (read-graph (seq input)))))))

(defn before? [s a b]
  "Predicate for two elements occurring in a sequence, one before the other"
  (let [i1 (.indexOf s a)
        i2 (.indexOf s b)]
    (and (>= i1 0) (>= i2 0) (< i1 i2))))

(defn is-valid-topsort [graph sort]
  "Assert that a given seq is a valid topological sort of a given graph"
  (let [expected-members (sets/union (set (keys graph)) (set (reduce concat (vals graph))))]
    ; Sort must contain precisely the set of nodes in the graph...
    (is (= expected-members (set sort)))
    ; ...each one exactly once...
    (is (= (count expected-members) (count sort)))
    ; ...and each node must appear after all its dependencies
    (doseq [[node deps] graph]
      (doseq [dep deps]
        (is (before? sort dep node))))))

(deftest topsort-test
  (doseq [graph [{}
                 {:a [:b]}
                 {:a [:b] :b [:c]}
                 {:a [:b :c] :b [:d] :c [:d :e]}]]
    (testing (str "Valid topological sort for " graph)
      (is-valid-topsort graph (topsort graph)))))

(deftest topsort-cycle
  (doseq [cyclic-graph [{:a [:b] :b [:a]}
                        {:a [:b] :b [:c] :c [:a]}
                        {:a [:b :c] :b [:z] :c [:d :e] :d [:e] :e [:c]}]]
    (testing (str "Test cyclic exception for " cyclic-graph)
      (is (thrown-with-msg? IllegalArgumentException #"Cycle detected"
                                      (topsort cyclic-graph))))))

; TODO Use test.check to randomly generate graphs and assert correct sorts for them
