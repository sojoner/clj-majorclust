(ns clj-majorclust.core
  (:require [loom.graph :refer :all]))

; a weighted graph
(def wg (weighted-graph {:a {:b 0.1 :c 0.2} :c {:d 0.3} :e {:b 0.05 :d 0.05}}))
 
(def maxweight (atom {:weight 0 :node nil})) 
(defn get-nearest-node [vertex g]    
  (swap! maxweight assoc :node vertex)
  (doseq [u (nodes g)]
    (cond (and (has-edge? g u vertex) (> (weight g u vertex) (:weight @maxweight)))   
            (do
              (swap! maxweight assoc :weight (weight g u vertex))
              (swap! maxweight assoc :node u))))
  (:node @maxweight))

(def assignment (atom {}))
(def termination (atom {:t false}))
(defn do-majorclust [graph]
  ; initial assignment
  (reset! assignment (into {} (map (fn [x y] {y x})(iterate inc 0) (nodes wg))))
  (swap! termination assoc :t false)
  (loop [g graph] 
     (if (not (:t @termination))
      ; relabel
      (do
          (swap! termination assoc :t true)
          (doseq [vertex (nodes g)]
            (let [nearest-neighbour (get-nearest-node vertex g)]
              (cond (not= (vertex @assignment) (nearest-neighbour @assignment))
                (do 
                  (swap! assignment assoc vertex (nearest-neighbour @assignment))                            
                  (swap! termination assoc :t false)))))
          @assignment)
      ; recure else
      (recur g))))
