(ns clj-graphics2d.adjacent-test
  (:require [clj-graphics2d.adjacent :refer :all]
            [clojure.test :refer :all]
            [clojure.core.matrix :as m]))

(deftest update-adjacencies-test
  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label -1)]
    (is (= (do
             (do-update 1)
             @current-label)
           1))
    (is (m/equals (do
                    (do-update 1)
                    @adjacencies)
                  (basic-adjacencies 3))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2)]
    (is (= (do
             (do-update 1)
             @current-label)
           1))
    (is (m/equals (do
                    (do-update 1)
                    @adjacencies)
                  (m/array [[-1  0  0]
                            [ 0 -1  1]
                            [ 0  1 -1]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2)]
    (is (= (do
             (do-update 0)
             @zero-count)
           1))
    (is (= (do
             (do-update 0)
             @current-label)
           2))
    (is (m/equals (do
                    (do-update 0)
                    @adjacencies)
                  (m/array [[-1  0  0]
                            [ 0 -1  0]
                            [ 0  0 -1]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2)]
    (is (= (do
             (do-update 0)
             (do-update 1)
             @current-label)
           1)))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2)]
    (is (m/e= (do
                (do-update 0)
                (do-update 1)
                @adjacencies)
              (m/array [[-1.0  0.0  0.0]
                        [ 0.0 -1.0  1.0]
                        [ 0.0  1.0 -1.0]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2)]
    (is (m/e= (do
                (do-update 0)
                (do-update 0)
                (do-update 1)
                @current-label)
              1))
    (is (m/e= (do
                (do-update 0)
                (do-update 0)
                (do-update 1)
                @adjacencies)
              (m/array [[-1.0  0.0  0.0]
                        [ 0.0 -1.0  0.0]
                        [ 0.0  0.0 -1.0]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2)]
    (is (m/e= (do
                (do-update 0)
                (do-update 0)
                (do-update 1)
                (do-update 0)
                (do-update 2)
                @adjacencies)
              (m/array [[-1.0  0.0  0.0]
                        [ 0.0 -1.0  1.0]
                        [ 0.0  1.0 -1.0]])))))

(deftest adjacency-test
  (is (m/equals (adjacencies (m/array [[1 1 1]
                                       [1 1 1]
                                       [1 1 1]]))
                (m/array [[-1  0]
                          [ 0 -1]])))

  (is (m/equals (adjacencies (m/array [[1 0 0]
                                       [0 0 0]
                                       [0 0 2]]))
                (m/array [[-1  0  0]
                          [ 0 -1  0]
                          [ 0  0 -1]])))

  #_(is (= (adjacencies (m/array [[1 0 2]
                                  [1 0 2]
                                  [1 0 2]]))
           (-> (basic-adjacencies 3)
               (m/mset 1 2 1)
               (m/mset 2 1 1)))))
