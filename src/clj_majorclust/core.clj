(ns clj-majorclust.core
  (:require [loom.graph :refer :all]))

; a weighted graph
(def wg (weighted-graph {:a {:b 0.1 :c 0.2} :c {:d 0.3} :e {:b 0.05 :d 0.05}}))

(defn argmax [vertex g]
  (loop [edges (filter (fn [u] (has-edge? g u vertex)) (nodes g))
         max_weight 0.0
         nearest vertex]
    (if (empty? edges)
      nearest
      (recur
        (rest edges)
        (if (> (weight g (first edges) vertex) max_weight) (weight g (first edges) vertex) max_weight)
        (if (> (weight g (first edges) vertex) max_weight) (first edges) nearest)))))

(defn do-majorclust [graph]
  (loop [g graph
         vertices (nodes g)
         assign-func (into {} (map (fn [x y] {y x}) (iterate inc 0) vertices))
         t false]
    (let [nearest (argmax (first vertices) g)]
      (if (empty? vertices)
        assign-func
        (recur
          g
          (rest vertices)
          (if (not= ((first vertices) assign-func) (nearest assign-func))
            (assoc assign-func (first vertices) (nearest assign-func))
            assign-func)
          (if (not= ((first vertices) assign-func) (nearest assign-func)) false true))))))