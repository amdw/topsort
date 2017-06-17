(ns topsort.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn -visit [node graph states result]
  "Depth-first search visit function for topological sort.
   A node's state is white initially, then grey once we have started to search its
   children, then finally black once it and all its (transitive) children have been visited."
  (let [state (get states node :white)]
    (when (= :grey state) (throw (IllegalArgumentException. (str "Cycle detected at " node))))
    (if (= :white state)
      (loop [new-states (assoc states node :grey)
             new-result result
             [child & rest-children] (get graph node [])]
        (if (nil? child) [(assoc new-states node :black) (conj new-result node)]
            (let [[new-states2 new-result2] (-visit child graph new-states new-result)]
              (recur new-states2 new-result2 rest-children))))
      [states result])))

(defn topsort [graph]
  "Produce a topological sort for a given graph."
  (loop [nodes (keys graph)
         states {}
         result []]
    (let [[node & remaining] nodes]
      (if (nil? node) result
          (let [state (get states node :white)
                [new-states new-result] (if (= :white state) (-visit node graph states result)
                                            [states result])]
            (recur remaining new-states new-result))))))

(defn read-graph [lines]
  "Read a graph from a seq of lines."
  (let [words (map #(str/split (str/trim %) #"\s+") lines)
        pairs (for [[node & deps] words :when (not (empty? node))]
                [(keyword node) (if (empty? deps) [] (map keyword deps))])]
    (into {} pairs)))

(defn -main
  "Read a graph from standard input and compute its topological sort."
  [& args]
  (let [graph (read-graph (line-seq (java.io.BufferedReader. *in*)))
        result (topsort graph)]
    (println "Sort:" (str/join " " (map name result)))))
