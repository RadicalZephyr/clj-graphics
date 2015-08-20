(ns clj-graphics2d.binary-morphology-test
  (:require [clj-graphics2d.binary-morphology :refer :all]
            [clojure.test :refer :all]))


(deftest rl-decode-test
  (testing "single bits"
   (is (= (rl-decode 0)
          [0]))
   (is (= (rl-decode 1)
          [1])))
  (testing "multiple specific bits"
    (is (= (rl-decode 0 1 0)
           [0 1 0])))
  (testing "single run"
    (is (= (rl-decode [2 0])
           [0 0]))
    (is (= (rl-decode [2 1])
           [1 1])))
  (testing "run followed by bits"
    (is (= (rl-decode [2 0] 1 0)
           [0 0 1 0])))
  (testing "comprehensive"
    (is (= (rl-decode 0 1 0 [4 1] 0 1 0)
           [0 1 0 1 1 1 1 0 1 0]))
    (is (= (rl-decode [4 1] 0 1 0 [4 1])
           [1 1 1 1 0 1 0 1 1 1 1]))))

(deftest rl-encode-test
  (testing "single bits"
    (is (= (rl-encode 0)
           [0]))
    (is (= (rl-encode 1)
           [1])))
  (testing "multiple specific bits"
    (is (= (rl-encode 0 1 0)
           [0 1 0])))
  (testing "single run"
    (is (= (rl-encode 0 0)
           [[2 0]]))
    (is (= (rl-encode 1 1)
           [[2 1]])))
  (testing "run followed by bits"
    (is (= (rl-encode 0 0 1 0)
           [[2 0] 1 0])))
  (testing "comprehensive"
    (is (= (rl-encode 0 1 0 1 1 1 1 0 1 0)
           [0 1 0 [4 1] 0 1 0]))
    (is (= (rl-encode 1 1 1 1 0 1 0 1 1 1 1)
           [[4 1] 0 1 0 [4 1]]))))

(deftest run-length-round-trip
  (let [bits [1 1 1 1 0 1 0 1 1 1 1]]
    (is (= (apply rl-decode (apply rl-encode bits))
           bits)))
  (let [encoded-bits [0 1 0 [4 1] 0 1 0]]
    (is (= (apply rl-encode (apply rl-decode encoded-bits))
           encoded-bits))))

(deftest structural-elements
  (let [st-el (make-custom-st [1 1]
                              :origin [0 0]
                              :dimensions [2 1])]
    (is (= (element st-el)
           [1 1]))
    (is (= (origin st-el)
           [0 0]))
    (is (= (dimensions st-el)
           [2 1]))
    (is (= (width st-el)
           2))
    (is (= (height st-el)
           1))))

(deftest binary-images
  (let [bimg (binary-image [2 2] [1 1 0 0])]
    (is (= (image bimg)
           [1 1 0 0]))
    (is (= (dimensions bimg)
           [2 2]))
    (is (= (width bimg)
           2))
    (is (= (height bimg)
           2))
    (is (= (with-out-str (display-image bimg))
           "1 1\n0 0\n"))))

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
