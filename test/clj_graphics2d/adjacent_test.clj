(ns clj-graphics2d.adjacent-test
  (:require [clj-graphics2d.adjacent :refer :all]
            [clojure.test :refer :all]
            [clojure.core.matrix :as m]))

(deftest update-adjacencies-test
  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label -1 :dimensions 2)]
    (do-update 2)
    (is (thrown? clojure.lang.ExceptionInfo
         (do-update 1)))

    (is (= @current-label
           2))
    (is (m/equals @adjacencies
                  (m/array [[-1  0]
                            [ 0 -1]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label -1 :dimensions 3)]
    (do-update 1)

    (is (= @current-label
           1))
    (is (m/equals @adjacencies
                  (m/array [[-1  0  0]
                            [ 0 -1  0]
                            [ 0  0 -1]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 1)

    (is (= @current-label
           1))
    (is (m/equals @adjacencies
                  (m/array [[-1  0  0]
                            [ 0 -1  1]
                            [ 0  1 -1]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)

    (is (= @zero-count
           1))
    (is (= @current-label
           2))
    (is (m/equals @adjacencies
                  (m/array [[-1  0  0]
                            [ 0 -1  0]
                            [ 0  0 -1]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)
    (do-update 1)

    (is (= @current-label
           1))
    (is (m/e= @adjacencies
              (m/array [[-1.0  0.0  0.0]
                        [ 0.0 -1.0  1.0]
                        [ 0.0  1.0 -1.0]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)
    (do-update 0)
    (do-update 1)

    (is (m/e= @current-label
              1))
    (is (m/e= @adjacencies
              (m/array [[-1.0  0.0  0.0]
                        [ 0.0 -1.0  0.0]
                        [ 0.0  0.0 -1.0]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)
    (do-update 0)
    (do-update 1)
    (do-update 0)
    (do-update 2)

    (is (m/e= @adjacencies
              (m/array [[-1.0  0.0  0.0]
                        [ 0.0 -1.0  1.0]
                        [ 0.0  1.0 -1.0]]))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label -1 :dimensions 3)]
    (do-update 1.0)
    (do-update 0.0)
    (do-update 0.0)
    (do-update 0.0)
    (do-update 0.0)
    (do-update 0.0)
    (do-update 0.0)
    (do-update 2.0)

    (is (m/e= @adjacencies
              (m/array [[-1.0  0.0  0.0]
                        [ 0.0 -1.0  0.0]
                        [ 0.0  0.0 -1.0]])))))

(deftest get-unique-labels-test
  (is (= (get-unique-labels (m/array [[1 1 1]
                                      [1 1 1]
                                      [1 1 1]]))
         #{0 1}))

  (is (= (get-unique-labels (m/array [[1 0 0]
                                      [0 0 0]
                                      [0 0 2]]))
         #{0 1 2}))

  (is (= (get-unique-labels (m/array [[1 1 1]
                                      [2 2 2]
                                      [3 3 3]]))
         #{0 1 2 3})))

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

  (is (= (adjacencies (m/array [[1 0 2]
                                [1 0 2]
                                [1 0 2]]))
         (m/array [[-1  0  0]
                   [ 0 -1  1]
                   [ 0  1 -1]])))

  (is (= (adjacencies (m/array [[1 1 1]
                                [0 0 0]
                                [2 2 2]]))
         (m/array [[-1  0  0]
                   [ 0 -1  1]
                   [ 0  1 -1]])))

  (is (= (adjacencies (m/array [[0 0 1]
                                [2 0 0]]))
         (m/array [[-1  0  0]
                   [ 0 -1  0]
                   [ 0  0 -1]])))

  (is (= (adjacencies (m/array [[0 1]
                                [0 0]
                                [2 0]]))
         (m/array [[-1  0  0]
                   [ 0 -1  0]
                   [ 0  0 -1]])))

  (is (= (adjacencies (m/array [[1 0 2 0 0]
                                [3 0 0 0 0]
                                [0 0 4 0 0]
                                [0 0 4 0 5]
                                [6 0 0 0 0]]))
         (m/array [[-1  0  0  0  0  0  0]
                   [ 0 -1  1  1  0  0  0]
                   [ 0  1 -1  0  1  0  0]
                   [ 0  1  0 -1  0  0  0]
                   [ 0  0  1  0 -1  1  0]
                   [ 0  0  0  0  1 -1  0]
                   [ 0  0  0  0  0  0 -1]]))))
