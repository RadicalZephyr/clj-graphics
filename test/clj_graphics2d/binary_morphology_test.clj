(ns clj-graphics2d.binary-morphology-test
  (:require [clj-graphics2d.binary-morphology :refer :all]
            [clojure.test :refer :all]))


(deftest run-length-encoding-test
  (testing "single bits"
   (is (= (run-length-encoding 0)
          [0]))
   (is (= (run-length-encoding 1)
          [1])))
  (testing "multiple specific bits"
    (is (= (run-length-encoding 0 1 0)
           [0 1 0])))
  (testing "single run"
    (is (= (run-length-encoding [2 0])
           [0 0]))
    (is (= (run-length-encoding [2 1])
           [1 1])))
  (testing "run followed by bits"
    (is (= (run-length-encoding [2 0] 1 0)
           [0 0 1 0])))
  (testing "comprehensive"
    (is (= (run-length-encoding 0 1 0 [4 1] 0 1 0)
           [0 1 0 1 1 1 1 0 1 0]))
    (is (= (run-length-encoding [4 1] 0 1 0 [4 1])
           [1 1 1 1 0 1 0 1 1 1 1]))))

(let [bimg (apply (comp vec concat)
                  '((0 0 0 0 0 0 0 0)
                    (1 1 1 1 1 1 1 0)
                    (0 0 0 1 1 1 1 0)
                    (0 0 0 1 1 1 1 0)
                    (0 0 1 1 1 1 1 0)
                    (0 0 0 1 1 1 1 0)
                    (0 0 1 1 0 0 0 0)
                    (0 0 0 0 0 0 0 0)))]

  (deftest basic-morphology
    (testing "dilation"
      (is (= (partition 8
                        (dilate bimg [8 8] {:origin [1 1] :dimensions [3 3]
                                            :element [1 1 1
                                                      1 1 1
                                                      1 1 1]}))
             '((1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1)
               (0 1 1 1 1 1 1 1)
               (0 1 1 1 1 1 1 1)
               (0 1 1 1 1 1 1 1)
               (0 1 1 1 1 1 1 1)
               (0 1 1 1 1 0 0 0)))))

    (testing "erosion"
      (is (= (partition 8
                        (erode bimg [8 8] {:origin [1 1] :dimensions [3 3]
                                           :element [1 1 1
                                                     1 1 1
                                                     1 1 1]}))
             '((0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0)
               (0 0 0 0 1 1 0 0)
               (0 0 0 0 1 1 0 0)
               (0 0 0 0 1 1 0 0)
               (0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0))))))

  (deftest composite-morphology
    (testing "closing"
      (is (= (partition 8
                        (close bimg [8 8] {:origin [1 1] :dimensions [3 3]
                                           :element [1 1 1
                                                     1 1 1
                                                     1 1 1]}))
             '((0 0 0 0 0 0 0 0)
               (0 1 1 1 1 1 1 0)
               (0 0 1 1 1 1 1 0)
               (0 0 1 1 1 1 1 0)
               (0 0 1 1 1 1 1 0)
               (0 0 1 1 1 1 1 0)
               (0 0 1 1 0 0 0 0)
               (0 0 0 0 0 0 0 0)))))

    (testing "opening"
      (is (= (partition 8
                        (open bimg [ 8 8] {:origin [1 1] :dimensions [3 3]
                                           :element [1 1 1
                                                     1 1 1
                                                     1 1 1]}))
             '((0 0 0 0 0 0 0 0)
               (0 0 0 1 1 1 1 0)
               (0 0 0 1 1 1 1 0)
               (0 0 0 1 1 1 1 0)
               (0 0 0 1 1 1 1 0)
               (0 0 0 1 1 1 1 0)
               (0 0 0 0 0 0 0 0)
               (0 0 0 0 0 0 0 0)))))))