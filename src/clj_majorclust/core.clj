(ns clj-majorclust.core
  (:require [loom.graph :refer :all]))

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
          (if (not= (get assign-func (first vertices)) (get assign-func nearest))
            (assoc assign-func (first vertices) (get assign-func nearest))
            assign-func)
          (if (not= (get assign-func (first vertices)) (get assign-func nearest)) false true))))))

(defn get-clusters [assign-func]
  (loop [keys (keys assign-func)
         result {}]
    (let [node-id (first keys)
          cluster-id (get assign-func node-id)]
      (if (empty? keys)
        result
        (recur
          (rest keys)
          (if (contains? result (keyword (str cluster-id)))
            (assoc result (keyword (str cluster-id)) (conj (get result (keyword (str cluster-id))) node-id))
            (assoc result (keyword (str cluster-id))  #{node-id}))))
      )
))