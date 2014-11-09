(ns clj-majorclust.importer
  (:require [loom.graph :refer :all]
            [clojure.java.io :as io]))

(defn- cosine-similarity
  "
  http://en.wikipedia.org/wiki/Cosine_similarity
  http://www.appliedsoftwaredesign.com/cosineSimilarityCalculator.php
  The Cosine Similarity of two vectors a and b is the ratio: a dot b / ||a|| ||b||
  Let d1 = {2 4 3 1 6}
  Let d2 = {3 5 1 2 5}
  Cosine Similarity (d1, d2) = dot(d1, d2) / ||d1|| ||d2||
  dot(d1, d2) = (2)*(3) + (4)*(5) + (3)*(1) + (1)*(2) + (6)*(5) = 61
  ||d1|| = sqrt((2)^2 + (4)^2 + (3)^2 + (1)^2 + (6)^2) = 8.12403840464
  ||d2|| = sqrt((3)^2 + (5)^2 + (1)^2 + (2)^2 + (5)^2) = 8
  Cosine Similarity (d1, d2) = 61 / (8.12403840464) * (8)
  = 61 / 64.9923072371
  = 0.938572618717
  "
  [a b]
  (let [counts
        (apply merge-with +
               (map
                 (fn [[x y]]
                   {:dot (* x y)
                    :a (Math/pow x 2)
                    :b (Math/pow y 2)})
                 (map vector a b)))]
    (/ (:dot counts)
       (* (Math/sqrt (:a counts))
          (Math/sqrt (:a counts))))))

(defn- line2Vector [line]
  (let [elements (clojure.string/split line #"\t")]
    (vector (read-string (nth elements 0)) (read-string (nth elements 1)))))

(defn- create-adjacent-map [vector all-vectors]
     (loop [node-ids (range (count all-vectors))
            nodes all-vectors
            adjacent-map {}]
       (if (empty? node-ids)
          adjacent-map
          (recur
            (rest node-ids)
            (rest nodes)
            (assoc adjacent-map  (first node-ids)  (cosine-similarity vector (first nodes)))))))

(defn import-txt-vector-file [path-to-file]
  (with-open [rdr (io/reader path-to-file)]
    (let [all-vectors (doall (map line2Vector (line-seq rdr)))]
          (loop [node-ids (range (count all-vectors))
                 graphmap {}]
            (if (empty? node-ids)
              (weighted-graph graphmap)
              (recur
                (rest node-ids)
                (assoc graphmap (first node-ids) (create-adjacent-map (nth all-vectors (first node-ids)) all-vectors))))))))