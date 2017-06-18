(ns topsort.core-test
  (:require [clojure.test :refer :all]
            [topsort.core :refer :all]
            [clojure.set :as sets]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

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

; Unfortunately this cannot use the (is) macro, as we want to reuse it in the test.check property
(defn valid-topsort? [graph sort]
  "Predicate for a given seq being a valid topological sort of a given graph"
  (let [expected-members (sets/union (set (keys graph)) (set (reduce concat (vals graph))))]
    (and 
     ; Sort must contain precisely the set of nodes in the graph...
     (= expected-members (set sort))
     ; ...each one exactly once...
     (= (count expected-members) (count sort))
     ; ...and each node must appear after all its dependencies
     (every? true? (for [[node deps] graph
                         dep deps]
                     (before? sort dep node))))))

(deftest topsort-test
  (doseq [graph [{}
                 {:a [:b]}
                 {:a [:b] :b [:c]}
                 {:a [:b :c] :b [:d] :c [:d :e]}]]
    (testing (str "Valid topological sort for " graph)
      (is (valid-topsort? graph (topsort graph))))))

(deftest topsort-cycle
  (doseq [cyclic-graph [{:a [:b] :b [:a]}
                        {:a [:b] :b [:c] :c [:a]}
                        {:a [:b :c] :b [:z] :c [:d :e] :d [:e] :e [:c]}]]
    (testing (str "Test cyclic exception for " cyclic-graph)
      (is (thrown-with-msg? IllegalArgumentException #"Cycle detected"
                                      (topsort cyclic-graph))))))

; test.check generator for random subset of a seq
(def gen-subset (fn [s] (gen/set (gen/elements s))))

; test.check generator for random graph.
(def gen-graph
  (let [gen-ordinals (gen/not-empty (gen/fmap (comp shuffle range inc) gen/nat))
        gen-order (gen/fmap #(map (comp keyword str char (partial + 97)) %) gen-ordinals)
        ; Graph nodes are simply integers from 0 to n
        dep-candidates (fn [order node] (take-while #(not= node %) order))
        ; Each node is allowed to depend on nodes *ahead* of it in the sequence
        gen-pair (fn [order node] (let [candidates (dep-candidates order node)]
                                    (gen/fmap #(vec [node %])
                                              (gen/fmap vec (gen-subset candidates)))))
        ; Generate a single key-value pair in the graph map
        gen-pairs (fn [order] (apply gen/tuple (map (partial gen-pair order) (rest order))))
        ; Generate all the pairs in the graph map... then drop them into a map
        gen-graph-with-order (fn [order] (gen/fmap #(into {} %) (gen-pairs order)))]
    (gen/bind gen-order gen-graph-with-order)))

(def gen-test-count 100)

; Generative test for topological sort validity.
(defspec topsort-gen-test gen-test-count
  (prop/for-all [graph gen-graph]
                (valid-topsort? graph (topsort graph))))
