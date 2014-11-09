(ns clj-majorclust.importer
  (:require [loom.graph :refer :all]
            [clojure.java.io :as io]
            [clj-majorclust.similaritys :as sim]))

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
            (assoc adjacent-map  (first node-ids)  (sim/cosine-similarity vector (first nodes)))))))

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