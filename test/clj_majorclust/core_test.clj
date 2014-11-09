(ns clj-majorclust.core-test
  (:require [clojure.test :refer :all]
            [clj-majorclust.core :refer :all]
            [loom.graph :refer :all]))

(deftest test-the-trival-weighted-graph
  (testing "Should cluster trival weighted graph"
    (let [graph (weighted-graph {:a {:b 0.1 :c 0.2} :c {:d 0.3} :e {:b 0.05 :d 0.05}})
          clustering (do-majorclust graph)]
      (println (get-clusters clustering))
      (is (= {:e 2, :c 3, :b 4, :d 3, :a 3} clustering)))))
