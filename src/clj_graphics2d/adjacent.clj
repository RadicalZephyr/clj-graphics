(ns clj-graphics2d.adjacent
  (:require [clojure.core.matrix :as m]))

(defn basic-adjacencies [dimensions]
  (m/emap #(if (= 1.0 %)
             (- %) %)
          (m/identity-matrix dimensions)))

(defn adjacencies [pixels]
  (let [unique-labels (distinct (conj (m/to-vector pixels) 0))
        num-labels    (count unique-labels)]
    (basic-adjacencies num-labels)))
