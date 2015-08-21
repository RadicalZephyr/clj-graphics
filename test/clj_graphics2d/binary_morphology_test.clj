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

(deftest structuring-elements
  (let [st-el (structuring-element [1 1]
                              :origin [1 0]
                              :dimensions [2 1])]
    (is (= (element st-el)
           [1 1]))
    (is (= (origin st-el)
           [1 0]))
    (is (= (origin-x st-el)
           1))
    (is (= (origin-y st-el)
           0))
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

(deftest structured-element-kernel-test
  (let [st-el (structuring-element [1]
                              :origin [0 0]
                              :dimensions [1 1])]
    (is (= (kernel-at st-el [0 0])
           [[0 0]]))
    (is (= (kernel-at st-el [0 1])
           [[0 1]])))
  (let [st-el (structuring-element [1 0]
                              :origin [0 0]
                              :dimensions [2 1])]
    (is (= (kernel-at st-el [0 0])
           [[0 0] [1 0]]))
    (is (= (kernel-at st-el [0 1])
           [[0 1] [1 1]])))
  (let [st-el (structuring-element [1 0]
                              :origin [0 0]
                              :dimensions [1 2])]
    (is (= (kernel-at st-el [0 0])
           [[0 0] [0 1]]))
    (is (= (kernel-at st-el [1 0])
           [[1 0] [1 1]]))))

(deftest extract-kernel-test
  (let [bimg (binary-image [2 2] [1 2 3 4])]
    (is (= (extract-kernel bimg [[0 0]])
           [1]))
    (is (= (extract-kernel bimg [[1 0]])
           [2]))
    (is (= (extract-kernel bimg [[0 0] [1 0]])
           [1 2]))
    (is (= (extract-kernel bimg [[0 1] [1 1]])
           [3 4]))
    (is (= (extract-kernel bimg [[0 0] [1 0] [0 1] [1 1]])
           [1 2 3 4]))))

;; (deftest dilation
;;   (let [bimg (binary-image [2 2] [1 0 0 0])
;;         st-el (structuring-element [1]
;;                               :origin [0 0]
;;                               :dimensions [1 1])]
;;     (is (= (dilate st-el bimg)
;;            bimg)))
;;   (let [bimg (binary-image [2 2] [1 0 0 0])
;;         st-el (structuring-element [1 1]
;;                               :origin [0 0]
;;                               :dimensions [2 1])]
;;     (is (= (dilate st-el bimg)
;;            (binary-image [2 2] [1 1 0 0])))))

;; (deftest erosion
;;   (let [bimg (binary-image [2 2] [1 1 0 0])
;;         st-el (structuring-element [1 1]
;;                               :origin [0 0]
;;                               :dimensions [2 1])]
;;     (is (= (erode st-el bimg)
;;            (binary-image [2 2] [1 0 0 0])))))

;; (deftest closure
;;   (let [bimg (binary-image [2 2] [1 0 0 0])
;;         st-el (structuring-element [1 1]
;;                               :origin [0 0]
;;                               :dimensions [2 1])]
;;     (is (= (close st-el bimg)
;;            bimg))))

;; (deftest opening
;;   (let [bimg (binary-image [2 2] [1 0 0 0])
;;         st-el (structuring-element [1 1]
;;                               :origin [0 0]
;;                               :dimensions [2 1])]
;;     (is (= (open st-el bimg)
;;            (binary-image [2 2] [0 0 0 0])))))

(deftest min-and-max-test
  (let [bimg (binary-image [2 2] [1 0 1 0])
        zero-image (binary-image [2 2] [0 0 0 0])]
    (is (= (min bimg zero-image)
           zero-image))
    (is (= (max bimg zero-image)
           bimg))))

;; (let [bimg (apply (comp vec concat)
;;                   '((0 0 0 0 0 0 0 0)
;;                     (1 1 1 1 1 1 1 0)
;;                     (0 0 0 1 1 1 1 0)
;;                     (0 0 0 1 1 1 1 0)
;;                     (0 0 1 1 1 1 1 0)
;;                     (0 0 0 1 1 1 1 0)
;;                     (0 0 1 1 0 0 0 0)
;;                     (0 0 0 0 0 0 0 0)))
;;       bimg (binary-image [8 8] bimg)
;;       st-el (structuring-element [1 1 1
;;                              1 1 1
;;                              1 1 1]
;;                             :origin [1 1]
;;                             :dimensions [3 3])]

;;   (deftest basic-morphology
;;     (testing "dilation"
;;       (is (= (partition 8
;;                         (image
;;                          (dilate st-el bimg)))
;;              '((1 1 1 1 1 1 1 1)
;;                (1 1 1 1 1 1 1 1)
;;                (1 1 1 1 1 1 1 1)
;;                (0 1 1 1 1 1 1 1)
;;                (0 1 1 1 1 1 1 1)
;;                (0 1 1 1 1 1 1 1)
;;                (0 1 1 1 1 1 1 1)
;;                (0 1 1 1 1 0 0 0)))))

;;     (testing "erosion"
;;       (is (= (partition 8
;;                         (image
;;                          (erode st-el bimg)))
;;              '((0 0 0 0 0 0 0 0)
;;                (0 0 0 0 0 0 0 0)
;;                (0 0 0 0 1 1 0 0)
;;                (0 0 0 0 1 1 0 0)
;;                (0 0 0 0 1 1 0 0)
;;                (0 0 0 0 0 0 0 0)
;;                (0 0 0 0 0 0 0 0)
;;                (0 0 0 0 0 0 0 0))))))

;;   (deftest composite-morphology
;;     (testing "closing"
;;       (is (= (partition 8
;;                         (image
;;                          (close st-el bimg)))
;;              '((0 0 0 0 0 0 0 0)
;;                (0 1 1 1 1 1 1 0)
;;                (0 0 1 1 1 1 1 0)
;;                (0 0 1 1 1 1 1 0)
;;                (0 0 1 1 1 1 1 0)
;;                (0 0 1 1 1 1 1 0)
;;                (0 0 1 1 0 0 0 0)
;;                (0 0 0 0 0 0 0 0)))))

;;     (testing "opening"
;;       (is (= (partition 8
;;                         (image
;;                          (open st-el bimg)))
;;              '((0 0 0 0 0 0 0 0)
;;                (0 0 0 1 1 1 1 0)
;;                (0 0 0 1 1 1 1 0)
;;                (0 0 0 1 1 1 1 0)
;;                (0 0 0 1 1 1 1 0)
;;                (0 0 0 1 1 1 1 0)
;;                (0 0 0 0 0 0 0 0)
;;                (0 0 0 0 0 0 0 0)))))))
