(ns clj-graphics2d.binary-morphology
  (:refer-clojure :rename {min core-min
                           max core-max})
  (:require [clj-graphics2d.util :refer [get2d assoc2d update2d
                                         updating-coll-by
                                         with-partials]]
            [clojure.string :as str]))

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

(defn make-custom-st [element & {:keys [dimensions origin]}]
  (let [[w h :as dim] (or dimensions
                          (calculate-dimensions element)
                          (throw (IllegalArgumentException.
                                  (str
                                   "Must specify the dimensions of the shape, "
                                   "either by directly passing them or passing "
                                   "a list of nested collections."))))]
    {:element (vec element)
     :dimensions dim
     :origin (or origin
                 [(quot w 2)
                  (quot h 2)])}))

(defn make-box-st [[w h :as dim] & {:keys [origin]}]
  (make-custom-st (vec (take (* w h) (repeat 1)))
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
    (make-custom-st (make-circle d :filled true)
                    :dimensions [d d]
                    :origin (or origin
                                [r r]))))

(defn make-ring-st [d & {:keys [origin]}]
  (let [r (quot d 2)]
    (make-custom-st (make-circle d :filled false)
                    :dimensions [d d]
                    :origin (or origin
                                [r r]))))

(defn element [structural-element]
  (:element structural-element))

(defn origin [structural-element]
  (:origin structural-element))

(defn origin-x [structural-element]
  (first (origin structural-element)))

(defn origin-y [structural-element]
  (second (origin structural-element)))

(defn dimensions [element]
  (:dimensions element))

(defn width [element]
  (first (dimensions element)))

(defn height [element]
  (second (dimensions element)))


;; ------------------------------------------------------------
;; Binary Images
;; ------------------------------------------------------------

(defn binary-image [dimensions image]
  {:image image
   :dimensions dimensions})

(defn image [bimg]
  (:image bimg))

(defn display-image [binary-image]
  (println (->> (image binary-image)
                (partition (width binary-image))
                (map #(str/join " " %))
                (str/join "\n"))))


;; ------------------------------------------------------------
;; Morphological Operations
;; ------------------------------------------------------------

(defn kernel-at [structural-element [x y]]
  (let [oy (origin-y structural-element)
        ox (origin-x structural-element)]
    (for [dy (range (height structural-element))
          dx (range (width structural-element))]
      [(+ x (- dx ox)) (+ y (- dy ox))])))

(defn is-in-bounds? [[w h] [x y]]
  (and (> w x -1)
       (> h y -1)))

(defn dilate-image [bimg [w h :as dim] st-el]
  (updating-coll-by [output-img (vec (take (count bimg) (repeat 0)))
                     pts (for [y (range h)
                               x (range w)] [x y])
                     :head-as pt
                     :when (= (get2d w bimg pt)
                              1)]
    (->> pt
         (kernel-at st-el)
         (map vector (:element st-el))
         (reduce (fn [img [val pt]]
                   (if (is-in-bounds? dim pt)
                     (update2d w img pt
                               bit-or val)
                     img))
                 output-img))))

(defn dilate [st-el bimg]
  (let [img-dimensions (dimensions bimg)
        img (image bimg)
        new-img (dilate-image img img-dimensions st-el)]
    (binary-image img-dimensions new-img)))

(defn erode-match [bimg [w _] {:keys [element] :as st-el} pt]
  (->> pt
       (kernel-at st-el)
       (map (fn [val pt]
              (if (= val 1)
                (= (get2d w bimg pt)
                   1)
                true)) element)
       (every? identity)))

(defn erode-image [bimg [w h :as dim] st-el]
  (updating-coll-by [output-img (vec (take (count bimg) (repeat 0)))
                     pts (for [y (range h)
                               x (range w)] [x y])
                     :head-as pt
                     :when (erode-match bimg dim st-el pt)]
    (assoc2d w output-img pt 1)))

(defn erode [st-el bimg]
  (let [img-dimensions (dimensions bimg)
        img (image bimg)
        new-img (erode-image img img-dimensions st-el)]
    (binary-image img-dimensions new-img)))

(defn close [st-el bimg]
  (->> bimg
       (dilate st-el)
       (erode st-el)))

(defn open [st-el bimg]
  (->> bimg
       (erode st-el)
       (dilate st-el)))

(defn min [img-a img-b]
  (if-let [img-dimensions (and (= (dimensions img-a)
                                  (dimensions img-b))
                               (dimensions img-b))]
    (let [img-a (image img-a)
          img-b (image img-b)
          new-img (map core-min img-a img-b)]
      (binary-image img-dimensions new-img))
    (throw (IllegalArgumentException.
            "Min requires images with the same dimensions."))))

(defn max [img-a img-b]
  (if-let [img-dimensions (and (= (dimensions img-a)
                                  (dimensions img-b))
                               (dimensions img-b))]
    (let [img-a (image img-a)
          img-b (image img-b)
          new-img (map core-max img-a img-b)]
      (binary-image img-dimensions new-img))
    (throw (IllegalArgumentException.
            "Max requires images with the same dimensions."))))

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

(defn get-morphological-op [op-key]
  (case op-key
    :dilate  dilate
    :erode   erode
    :open    open
    :close   close
    identity))
