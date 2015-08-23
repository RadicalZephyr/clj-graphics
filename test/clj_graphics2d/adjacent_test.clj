(ns clj-graphics2d.adjacent-test
  (:require [clj-graphics2d.adjacent :refer :all]
            [clojure.test :refer :all]
            [clojure.core.matrix :as m]))

(deftest adjacency-test
  (is (= (adjacencies (m/array
                       [[1 1 1]
                        [1 1 1]
                        [1 1 1]]))
         (basic-adjacencies 2)))

  (is (= (adjacencies (m/array [[1 0 0]
                                [0 0 0]
                                [0 0 2]]))
         (basic-adjacencies 3))))
