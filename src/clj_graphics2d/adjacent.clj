(ns clj-graphics2d.adjacent
  (:require [clojure.core.matrix :as m]))

(defn basic-adjacencies [dimensions]
  (m/emap #(if (= 1.0 %)
             (- %) %)
          (m/mutable (m/identity-matrix dimensions))))

(defn update-adjacencies [adjacencies current-label next-label]
  (dosync
   (when (not= next-label 0)
     (let [cl @current-label]
       (when (and (not= cl next-label)
                  (not= cl -1))
         (alter adjacencies
                #(-> %
                     (m/mset cl next-label 1.0)
                     (m/mset next-label cl 1.0)))))
     (ref-set current-label next-label))))

(defn adjacencies [pixels]
  (let [unique-labels (distinct (conj (m/to-vector pixels) 0))
        num-labels    (count unique-labels)
        [height width] (m/shape pixels)
        adjacencies (basic-adjacencies num-labels)]
    adjacencies))
