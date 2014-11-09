(ns clj-majorclust.similaritys)

(quote "
Copyright (c) Bradford Cross and Hamilton Ulmer released under the MIT License
* https://github.com/aria42/infer")

(quote "from @Sojoner I simply use some measure function to make live easy.")

(defn tree-comp-each [root branch & leaves]
  (apply
    root (map branch leaves)))

(defn cosine-similarity
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

(defn minkowski-distance
  "http://en.wikipedia.org/wiki/Minkowski_distance
  http://en.wikipedia.org/wiki/Lp_space
  The Minkowski distance is a metric on Euclidean space which can be considered as a generalization of both the Euclidean distance and the Manhattan distance.
  Minkowski distance is typically used with p being 1 or 2. The latter is the Euclidean distance, while the former is sometimes known as the Manhattan distance.
  `
  In the limiting case of p reaching infinity we obtain the Chebyshev distance."
  [a b p]
  (let [_ (assert (= (count a) (count b)))]
    (Math/pow
      (apply
        tree-comp-each
        +
        (fn [[x y]]
          (Math/pow
            (Math/abs
              (- x y))
            p))
        (map vector a b))
      (/ 1 p))))

(defn euclidean-distance
  "http://en.wikipedia.org/wiki/Euclidean_distance
  the Euclidean distance or Euclidean metric is the ordinary distance between two points that one would measure with a ruler, and is given by the Pythagorean formula. By using this formula as distance, Euclidean space (or even any inner product space) becomes a metric space. The associated norm is called the Euclidean norm. Older literature refers to the metric as Pythagorean metric."
  [a b]
  (minkowski-distance a b 2))

(defn dot-product
  [x y]
  (apply + (map * x y)))

(defn chebyshev-distance
  "In the limiting case of Lp reaching infinity we obtain the Chebyshev distance."
  [a b]
  (let [_ (assert (= (count a) (count b)))]
    (apply
      tree-comp-each
      max
      (fn [[x y]] (- x y))
      (map vector a b))))

(defn manhattan-distance
  "http://en.wikipedia.org/wiki/Manhattan_distance
  usual metric of Euclidean geometry is replaced by a new metric in which the distance between two points is the sum of the (absolute) differences of their coordinates. The taxicab metric is also known as rectilinear distance, L1 distance or l1 norm (see Lp space), city block distance, Manhattan distance, or Manhattan length"
  [a b]
  (minkowski-distance a b 1))