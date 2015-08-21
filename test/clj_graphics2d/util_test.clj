(ns clj-graphics2d.util-test
  (:require [clojure.test                 :refer :all]
            [clj-graphics2d.util :refer :all])
  (:import java.awt.image.BufferedImage))

(deftest with-partials-test
  (testing "make-partial-binding"
    (is (= (make-partial-binding '[thing arg])
           '(thing (clojure.core/partial thing arg))))
    (is (= (make-partial-binding '[thing [arg1 arg2]])
           '(thing (clojure.core/partial thing arg1 arg2))))
    (is (= (make-partial-binding '[[fun1 fun2] arg])
           '[fun1 (clojure.core/partial fun1 arg)
             fun2 (clojure.core/partial fun2 arg)]))
    (is (= (make-partial-binding '[[fun1 fun2] [arg1 arg2]])
           '[fun1 (clojure.core/partial fun1 arg1 arg2)
             fun2 (clojure.core/partial fun2 arg1 arg2)])))

  (testing "make-partial-bindings"
    (is (= (make-partial-bindings ['thing 'arg])
           '[thing (clojure.core/partial thing arg)]))
    (is (= (make-partial-bindings '[fake-fun [arg1 arg2 arg3]])
           '[fake-fun (clojure.core/partial fake-fun arg1 arg2 arg3)]))
    (is (= (make-partial-bindings '[[fun1 fun2] arg])
           '[fun1 (clojure.core/partial fun1 arg)
             fun2 (clojure.core/partial fun2 arg)]))
    (is (= (make-partial-bindings '[thing       arg
                            [fun1 fun2] [arg1 arg2]])
           '[thing (clojure.core/partial thing arg)
             fun1 (clojure.core/partial fun1 arg1 arg2)
             fun2 (clojure.core/partial fun2 arg1 arg2)])))

  (testing "with-partials"
    (is (= (macroexpand-1 '(with-partials [open st-el]
                             (dummy-body (open img))))
           '(clojure.core/let [open (clojure.core/partial open st-el)]
              (dummy-body (open img)))))
    (is (= (macroexpand-1 '(with-partials [[open close] st-el]
                             (dummy-body (open img))))
           '(clojure.core/let [open (clojure.core/partial open st-el)
                               close (clojure.core/partial close st-el)]
              (dummy-body (open img)))))))

(deftest op2d-test
  (testing "get2d"
    (is (= (get2d 4 [1 2 3 4
                     5 6 7 8] [0 0])
           1))
    (is (= (get2d 4 [1 2 3 4
                     5 6 7 8] [0 1])
           5))
    (is (= (get2d 4 [1 2 3 4
                     5 6 7 8] [1 0])
           2))
    (is (= (get2d 4 [1 2 3 4
                     5 6 7 8] [1 1])
           6)))
  (testing "assoc2d"
    (testing "single update"
      (is (= (assoc2d 4 [1 2 3 4
                         5 6 7 8] [0 0] 11)
             [11 2 3 4
              5 6 7 8]))
      (is (= (assoc2d 4 [1 2 3 4
                         5 6 7 8] [0 1] 11)
             [1  2 3 4
              11 6 7 8]))
      (is (= (assoc2d 4 [1 2 3 4
                         5 6 7 8] [1 0] 11)
             [1 11 3 4
              5 6  7 8]))
      (is (= (assoc2d 4 [1 2 3 4
                         5 6 7 8] [1 1] 11)
             [1 2  3 4
              5 11 7 8])))
    (testing "multiple update"
      (is (= (assoc2d 4 [1 2 3 4
                         5 6 7 8]
                      [0 0] 11
                      [0 1] 11
                      [1 0] 11
                      [1 1] 11
                      )
             [11 11  3 4
              11 11 7 8]))))
  (testing "update2d"
    (testing "no extra args"
     (is (= (update2d 4 [1 2 3 4
                         5 6 7 8] [0 0] inc)
            [2 2 3 4
             5 6 7 8]))
     (is (= (update2d 4 [1 2 3 4
                         5 6 7 8] [0 1] inc)
            [1 2 3 4
             6 6 7 8]))
     (is (= (update2d 4 [1 2 3 4
                         5 6 7 8] [1 0] inc)
            [1 3 3 4
             5 6 7 8]))
     (is (= (update2d 4 [1 2 3 4
                         5 6 7 8] [1 1] inc)
            [1 2 3 4
             5 7 7 8])))
    (testing "with extra args"
      (is (= (update2d 4 [1 2 3 4
                          5 6 7 8] [0 0] + 2)
             [3 2 3 4
              5 6 7 8]))
      (is (= (update2d 4 [1 2 3 4
                          5 6 7 8] [0 1] + 1 1)
             [1 2 3 4
              7 6 7 8]))
      (is (= (update2d 4 [1 2 3 4
                          5 6 7 8] [1 0] + 0 1 1)
             [1 4 3 4
              5 6 7 8]))
      (is (= (update2d 4 [1 2 3 4
                          5 6 7 8] [1 1] + 1 0 1 0)
             [1 2 3 4
              5 8 7 8])))))

(deftest get-pixels-test
  (testing "Nil returns"
    (is (= (get-pixels nil 0 0 0 1)
           []))
    (is (= (get-pixels nil 0 0 1 0)
           []))
    (is (= (get-pixels nil 0 0 0 0)
           [])))

  (testing "Get pixel vectors from an image"
    (doseq [img-type [BufferedImage/TYPE_INT_RGB
                      BufferedImage/TYPE_INT_ARGB
                      BufferedImage/TYPE_3BYTE_BGR
                      BufferedImage/TYPE_4BYTE_ABGR]]
      (let [img (BufferedImage. 10 10 img-type)]
        (is (= (class [])
               (class (get-pixels img 0 0 10 10))))))))

(deftest set-pixels-test
  (testing "Nil returns"
    (is (= (set-pixels nil 0 0 0 1 [])
           nil))
    (is (= (set-pixels nil 0 0 1 0 [])
           nil))
    (is (= (set-pixels nil 0 0 0 0 [])
           nil)))

  (testing "Set pixel of image an image from vector"
    (doseq [img-type [BufferedImage/TYPE_INT_RGB
                      BufferedImage/TYPE_INT_ARGB
                      BufferedImage/TYPE_3BYTE_BGR
                      BufferedImage/TYPE_4BYTE_ABGR]]
      (let [img (BufferedImage. 10 10 img-type)]
        (is (= img
               (set-pixels img 0 0 2 2 [1 0 1 0])))))))

(def str-type
  {BufferedImage/TYPE_INT_RGB        "INT_RGB"
   BufferedImage/TYPE_INT_ARGB       "INT_ARGB"
   BufferedImage/TYPE_INT_ARGB_PRE   "INT_ARGB_PRE"
   BufferedImage/TYPE_3BYTE_BGR      "3BYTE_BGR"
   BufferedImage/TYPE_4BYTE_ABGR     "4BYTE_ABGR"
   BufferedImage/TYPE_4BYTE_ABGR_PRE "4BYTE_ABGR_PRE"
   BufferedImage/TYPE_USHORT_565_RGB "USHORT_565_RGB"
   BufferedImage/TYPE_USHORT_555_RGB "USHORT_555_RGB"
   BufferedImage/TYPE_BYTE_GRAY      "BYTE_GRAY"
   BufferedImage/TYPE_USHORT_GRAY    "USHORT_GRAY"
   BufferedImage/TYPE_BYTE_BINARY    "BYTE_BINARY"
   BufferedImage/TYPE_BYTE_INDEXED   "BYTE_INDEXED"})

(deftest round-trip-test
  (testing "Starting with a vector"
    (doseq [img-type [BufferedImage/TYPE_INT_RGB
                      BufferedImage/TYPE_INT_ARGB
                      BufferedImage/TYPE_4BYTE_ABGR]
            pixels [[1 2 3 4]
                    [4 3 2 1]
                    [0 1 0 1]
                    [1 0 1 0]]
            :let [img (BufferedImage. 2 2 img-type)]]
      (testing (str pixels " and type " (str-type img-type))
       (is (= pixels
              (-> img
                  (set-pixels 0 0 2 2 pixels)
                  (get-pixels 0 0 2 2))))))))
