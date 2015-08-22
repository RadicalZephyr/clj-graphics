(ns clj-graphics2d.binary-morphology
  (:refer-clojure :rename {min core-min
                           max core-max})
  (:require [clj-graphics2d.util :refer [with-partials]]
            [clojure.string :as str]
            [clojure.core.matrix :as m]))

;; ------------------------------------------------------------
;; Run-Length Encoding/Decoding
;; ------------------------------------------------------------

(defn rl-decode [& codes]
  (let [not-sequential? (complement sequential?)]
    (into []
          (loop [acc []
                 codes (seq codes)]
            (if (seq codes)
              (if (sequential? (first codes))
                (let [[n value] (first codes)]
                  (recur (concat acc (take n (repeat value)))
                         (rest codes)))
                (recur (concat acc (take-while not-sequential? codes))
                       (drop-while not-sequential? codes)))
              acc)))))

(defn rl-encode [& codes]
  (loop [acc   []
         codes codes]
    (if (seq codes)
      (let [is-first? #(= (first codes) %)
            same-count (count (take-while is-first? codes))]
        (if (> same-count 1)
          (recur (conj acc [same-count (first codes)])
                 (drop-while is-first? codes))
          (recur (conj acc (first codes))
                 (rest codes))))
      acc)))


;; ------------------------------------------------------------
;; Structural Elements
;; ------------------------------------------------------------

(defn- calculate-dimensions [element]
  (let [head (first element)]
    (when (counted? head)
      [(count head)
       (count element)])))

(defn structuring-element [element & {:keys [dimensions origin]}]
  (let [[w h :as dim] (or dimensions
                          (throw (IllegalArgumentException.
                                  "Must specify the dimensions of the shape.")))]
    {:element (m/array (partition w element))
     :dimensions dim
     :origin (or origin
                 [(quot w 2)
                  (quot h 2)])}))

(defn make-box-st [[w h :as dim] & {:keys [origin]}]
  (structuring-element (vec (take (* w h) (repeat 1)))
                       :dimensions dim
                       :origin (or origin
                                   [(quot w 2)
                                    (quot h 2)])))

(defn- square [x]
  (* x x))

(defn- distance-sq2d [[x1 y1] [x2 y2]]
  (+ (square (- x2 x1))
     (square (- y2 y1))))

(defn make-circle [d & {:keys [filled]
                        :or {filled true}}]
  (let [r (+ (quot d 2)
             (if (even? d) -0.5 0))
        rq-sq (- (square r) 2)
        r-sq (int (square (/ d 2)))
        center [r r]]
    (for [y (range d)
          x (range d)
          :let [distance (distance-sq2d
                          center [x y])]]
      (if (and (>= r-sq distance)
               (or filled
                   (< rq-sq distance)))
        1
        0))))

(defn make-disk-st [d & {:keys [origin]}]
  (let [r (quot d 2)]
    (structuring-element (make-circle d :filled true)
                         :dimensions [d d]
                         :origin (or origin
                                     [r r]))))

(defn make-ring-st [d & {:keys [origin]}]
  (let [r (quot d 2)]
    (structuring-element (make-circle d :filled false)
                         :dimensions [d d]
                         :origin (or origin
                                     [r r]))))

(defn element [structuring-element]
  (flatten (:element structuring-element)))

(defn origin [structuring-element]
  (:origin structuring-element))

(defn origin-x [structuring-element]
  (first (origin structuring-element)))

(defn origin-y [structuring-element]
  (second (origin structuring-element)))

(defn dimensions [element]
  (->> element
       :element
       m/shape
       reverse
       vec))

(defn width [element]
  (first (dimensions element)))

(defn height [element]
  (second (dimensions element)))


;; ------------------------------------------------------------
;; Binary Images
;; ------------------------------------------------------------

(defn binary-image [[w h] image]
  {:element (m/array (partition w image))})

(defn image [bimg]
  (flatten (:element bimg)))

(defn display-image [binary-image]
  (println (->> (image binary-image)
                (replace {0 \` 1 \@})
                (partition (width binary-image))
                (map #(str/join " " %))
                (str/join "\n"))))

(defn get-in-image [bimg [x y]]
  (m/mget (:element bimg) y x))

(defn update-image-at [bimg [x y] value]
  (assoc bimg :element
         (m/mset (:element bimg) y x value)))


;; ------------------------------------------------------------
;; Morphological Operations
;; ------------------------------------------------------------

(defn kernel-at [structuring-element [x y]]
  (let [oy (origin-y structuring-element)
        ox (origin-x structuring-element)]
    (for [dy (range (height structuring-element))
          dx (range (width structuring-element))]
      [(+ x (- dx ox)) (+ y (- dy oy))])))

(defn is-in-bounds? [[w h] [x y]]
  (and (> w x -1)
       (> h y -1)))

(defn extract-kernel [bimg kernel]
  (mapv #(if (is-in-bounds? (dimensions bimg) %)
           (get-in-image bimg %)
           0)
        kernel))

(defn structure= [st-bit probe-bit]
  (or (= st-bit 0)
      (= st-bit probe-bit)))

(defn fit? [st-el probe]
  (every? #(apply structure= %) (map vector (element st-el) probe)))

(defn hit? [st-el probe]
  (boolean (some #(apply structure= %) (map vector (element st-el) probe))))

(defn binary-bit-op [op img-a img-b]
  (if-let [img-dimensions (and (= (dimensions img-a)
                                  (dimensions img-b))
                               (dimensions img-b))]
    (let [img-a (image img-a)
          img-b (image img-b)
          new-img (mapv op img-a img-b)]
      (binary-image img-dimensions new-img))
    (throw (IllegalArgumentException.
            "Binary image operations require images with the same dimensions."))))

(def min (partial binary-bit-op core-min))

(def max (partial binary-bit-op core-max))

(def intersection (partial binary-bit-op
                           (fn [f g]
                             (if (= 1 f g) 1 0))))

(def union (partial binary-bit-op
                    (fn [f g]
                      (if (or (= f 1)
                              (= g 1))
                        1 0))))

(def difference (partial binary-bit-op
                         (fn [f g]
                           (if (= f 1)
                             (- f g) 0))))

(defmacro def-morph-op [name predicate]
  `(defn ~name [st-el# orig-image# mod-image# coords#]
     (let [kernel# (kernel-at st-el# coords#)
           probe#  (extract-kernel orig-image# kernel#)]
       (if (~predicate st-el# probe#)
         (update-image-at mod-image# coords# 1)
         (update-image-at mod-image# coords# 0)))))

(def-morph-op dilate-at hit?)
(def-morph-op  erode-at fit?)

(defn four-kernel-at [[x y]]
  [[x  (dec y)] [(dec x) y]
   [(inc x) y]  [x  (inc y)]])

(defn remove-nub-at [orig-image mod-image coords]
  (let [kernel (four-kernel-at coords)
        probe  (extract-kernel orig-image kernel)
        probe-stats (frequencies probe)
        center-bit (get-in-image orig-image coords)]
    (if (and (= center-bit 1)
             (= probe-stats {0 3 1 1}))
      (update-image-at mod-image coords 0)
      mod-image)))

(defn make-process-with [at-fn]
  (fn [st-el bimg]
    (->> (for [y (range (height bimg))
               x (range (width bimg))] [x y])
         (reduce (partial at-fn st-el bimg) bimg))))

(def dilate (make-process-with dilate-at))
(def erode  (make-process-with  erode-at))

(defn remove-nubs [bimg]
  (->> (for [y (range (height bimg))
             x (range (width bimg))] [x y])
       (reduce (partial remove-nub-at bimg) bimg)))

(defn conditional-dilation [st-el bimg cimg]
  (loop [prev-img bimg
         c-img cimg]
    (if (= prev-img c-img)
      c-img
      (recur c-img (intersection bimg
                                 (dilate st-el c-img))))))

(defn close [st-el bimg]
  (->> bimg
       (dilate st-el)
       (erode st-el)))

(defn open [st-el bimg]
  (->> bimg
       (erode st-el)
       (dilate st-el)))

(defn proper-opening [st-el bimg]
  (with-partials [[open close] st-el]
    (min bimg (close (open (close bimg))))))

(defn proper-closing [st-el bimg]
  (with-partials [[open close] st-el]
    (max bimg (open (close (open bimg))))))

(defn automedian-filter [st-el bimg]
  (with-partials [[open close] st-el]
    (max (open (close (open bimg)))
         (min bimg
              (close (open (close bimg)))))))

(def vt-ln (structuring-element [1 1 1] :origin [0 1] :dimensions [1 3]))
(def hz-ln (structuring-element [1 1 1] :origin [1 0] :dimensions [3 1]))

(def sq2-se-d (structuring-element [1 1 1 1] :dimensions [2 2] :origin [0 0]))
(def sq2-se-e (structuring-element [1 1 1 1] :dimensions [2 2] :origin [1 1]))

(defn erode-2sq [bimg]
  (erode sq2-se-e bimg))

(defn dilate-2sq [bimg]
  (dilate sq2-se-d bimg))

(def sq3-se (structuring-element [1 1 1 1 1 1 1 1 1] :dimensions [3 3] :origin [1 1]))

(def bimg (binary-image [18 13]
                        [0 0 0 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0
                         1 1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 0 0
                         0 0 0 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1
                         0 0 0 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1
                         0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1
                         0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0
                         0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0
                         1 1 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0
                         0 0 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1
                         0 0 1 1 1 1 1 0 0 0 0 1 1 1 0 0 0 0
                         0 0 1 1 1 1 1 0 0 0 0 1 1 1 0 0 0 0
                         0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0 0
                         0 0 0 0 0 0 1 0 0 0 0 1 1 1 0 0 0 0]))

(defn vertical-lines [bimg]
  (open vt-ln bimg))

(defn horizontal-lines [bimg]
  (open hz-ln bimg))

(defn thin-lines [bimg]
  (let [fat-line-cores (erode  sq3-se bimg)
        fat-lines      (dilate sq3-se fat-line-cores)
        no-fat         (difference bimg fat-lines)

        med-line-cores (erode-2sq no-fat)
        med-lines      (dilate-2sq med-line-cores)
        no-fat-med     (difference no-fat med-lines)

        thincomplete-lines (-> no-fat-med
                               (union fat-line-cores)
                               (union med-line-cores))]
    (union (conditional-dilation vt-ln bimg (vertical-lines thincomplete-lines))
           (conditional-dilation hz-ln bimg (horizontal-lines thincomplete-lines)))))
