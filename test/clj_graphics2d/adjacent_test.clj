(ns clj-graphics2d.adjacent-test
  (:require [clj-graphics2d.adjacent :refer :all]
            [clojure.test :refer :all]
            [clojure.core.matrix :as m]))

(deftest adjacent?-test
  (testing "PersistentMap"
   (is (= (adjacent? {} 1 2)
          false))
   (is (= (adjacent? {1 [2]
                      2 [1]} 1 2)
          false)))
  (testing "Matrix"
    (is (= (adjacent? (m/array [[-1  0  0]
                                [ 0 -1  0]
                                [ 0  0 -1]])
                      1 2)
           false))
    (is (= (adjacent? (m/array [[-1  0  0]
                                [ 0 -1  1]
                                [ 0  1 -1]])
                      1 2)
           true))))

(deftest update-adjacencies-test
  (testing "error conditions"
    (let [{:keys [adjacencies zero-count current-label do-update]}
          (make-update-adjacencies :initial-label -1 :dimensions 2)]
      (do-update 2)
      (is (thrown? clojure.lang.ExceptionInfo
                   (do-update 1)))

      (is (= @current-label
             2))
      (is (thrown? clojure.lang.ExceptionInfo
                   (adjacent? @adjacencies 1 2)))))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label -1 :dimensions 3)]
    (do-update 1)

    (is (= @current-label
           1))
    (is (= (adjacent? @adjacencies 0 1)
           false))
    (is (= (adjacent? @adjacencies 1 0)
           false))
    (is (= (adjacent? @adjacencies 1 2)
           false))
    (is (= (adjacent? @adjacencies 2 1)
           false)))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 1)

    (is (= @current-label
           1))
    (is (= (adjacent? @adjacencies 1 2)
           true)))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)

    (is (= @zero-count
           1))
    (is (= @current-label
           2))
    (is (= (adjacent? @adjacencies 1 2)
           false)))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)
    (do-update 1)

    (is (= @current-label
           1))
    (is (= (adjacent? @adjacencies 1 2)
           true)))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)
    (do-update 0)
    (do-update 1)

    (is (m/e= @current-label
              1))
    (is (= (adjacent? @adjacencies 1 2)
           false)))

  (let [{:keys [adjacencies zero-count current-label do-update]}
        (make-update-adjacencies :initial-label 2 :dimensions 3)]
    (do-update 0)
    (do-update 0)
    (do-update 1)
    (do-update 0)
    (do-update 2)

    (is (= (adjacent? @adjacencies 1 2)
           true)))

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

    (is (= (adjacent? @adjacencies 1 2)
           false))))

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
  (is (= (adjacent? (adjacencies (m/array [[1 1 1]
                                           [1 1 1]
                                           [1 1 1]]))
                    0 1)
         false))

  (let [pixels (m/array [[1 0 0]
                         [0 0 0]
                         [0 0 2]])]
    (is (= (adjacent? (adjacencies pixels) 0 1)
           (adjacent? (adjacencies pixels) 1 2)
           false)))

  (let [pixels (m/array [[1 0 2]
                         [1 0 2]
                         [1 0 2]])]
    (is (= (adjacent? (adjacencies pixels) 0 1)
           false))
    (is (= (adjacent? (adjacencies pixels) 1 2)
           true)))

  (let [pixels (m/array  [[1 1 1]
                          [0 0 0]
                          [2 2 2]])]
    (is (= (adjacent? (adjacencies pixels) 0 1)
           false))
    (is (= (adjacent? (adjacencies pixels) 1 2)
           true)))

  (let [pixels (m/array [[0 0 1]
                         [2 0 0]])]
    (is (= (adjacent? (adjacencies pixels) 0 1)
           (adjacent? (adjacencies pixels) 1 2)
           false)))

  (let [pixels (m/array  [[0 1]
                          [0 0]
                          [2 0]])]
    (is (= (adjacent? (adjacencies pixels) 0 1)
           (adjacent? (adjacencies pixels) 1 2)
           false)))

  (let [pixels (m/array [[1 0 2 0 0]
                         [3 0 0 0 0]
                         [0 0 4 0 0]
                         [0 0 4 0 5]
                         [6 0 0 0 0]])]
    (is (= (adjacent? (adjacencies pixels) 1 2)
           (adjacent? (adjacencies pixels) 1 3)
           (adjacent? (adjacencies pixels) 2 4)
           (adjacent? (adjacencies pixels) 4 5)
           true))
    (let [adjs (adjacencies pixels)
          valid-pairs #{#{1 2}
                        #{1 3}
                        #{2 4}
                        #{4 5}}
          all-pairs (for [i (range 7)
                          j (range 7)
                          :when (not= i j)] #{i j})
          both-pairs (group-by (comp boolean valid-pairs)
                               all-pairs)
          invalid-pairs (get both-pairs false)]
      (doseq [[i j] (map vec invalid-pairs)]
        (is (= (adjacent? adjs i j)
               (adjacent? adjs j i)
               false))))))
