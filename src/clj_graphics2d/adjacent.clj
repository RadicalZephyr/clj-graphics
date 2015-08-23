(ns clj-graphics2d.adjacent
  (:require [clojure.core.matrix :as m]))

(defn adjacencies [pixels]
  (let [unique-labels (remove #{0}
                              (distinct (m/to-vector pixels)))]
    (m/identity-matrix (count unique-labels))))
