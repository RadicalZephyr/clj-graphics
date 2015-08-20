(ns clj-graphics2d.binary-morphology
  (:require [clojure.pprint :refer [pprint]]
            [clj-graphics2d.util :refer [get2d assoc2d update2d
                                         updating-coll-by]]))

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

(defn display-image [width img]
  (pprint (partition width img)))

(defn display-rl-image [width rl-img]
  (display-image (rl-decode rl-img)))

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

(defn dimensions [structural-element]
  (:dimensions structural-element))

(defn origin [structural-element]
  (:origin structural-element))

(defn get-kernel-from-st-el [{[ox oy] :origin
                              [w   h] :dimensions} [x y]]
  (for [dy (range h)
        dx (range w)]
    [(+ x (- dx ox)) (+ y (- dy oy))]))

(defn is-in-bounds? [[w h] [x y]]
  (and (> w x -1)
       (> h y -1)))

(defn dilate [bimg [w h :as dim] st-el]
  (updating-coll-by [output-img (vec (take (count bimg) (repeat 0)))
                          pts (for [y (range h)
                                    x (range w)] [x y])
                          :head-as pt
                          :when (= (get2d w bimg pt)
                                   1)]
    (->> pt
         (get-kernel-from-st-el st-el)
         (map vector (:element st-el))
         (reduce (fn [img [val pt]]
                   (if (is-in-bounds? dim pt)
                     (update2d w img pt
                                    bit-or val)
                     img))
                 output-img))))

(defn erode-match [bimg [w _] {:keys [element] :as st-el} pt]
  (->> pt
       (get-kernel-from-st-el st-el)
       (map (fn [val pt]
              (if (= val 1)
                (= (get2d w bimg pt)
                   1)
                true)) element)
       (every? identity)))

(defn erode [bimg [w h :as dim] st-el]
  (updating-coll-by [output-img (vec (take (count bimg) (repeat 0)))
                         pts (for [y (range h)
                                   x (range w)] [x y])
                         :head-as pt
                         :when (erode-match bimg dim st-el pt)]
    (assoc2d w output-img pt 1)))

(defn close [bimg dim st-el]
  (-> bimg
   (dilate dim st-el)
   (erode dim st-el)))

(defn open [bimg dim st-el]
  (-> bimg
   (erode dim st-el)
   (dilate dim st-el)))

(defn get-morphological-op [op-key]
  (case op-key
    :dilate  dilate
    :erode   erode
    :open    open
    :close   close
    identity))




(defn make-st-el [type arg & {:as opt}]
  (case type
    :box     (apply make-box-st arg opt)
    :disk    (apply make-disk-st arg opt)
    :ring    (apply make-ring-st arg opt)
    :custom  (apply make-custom-st arg opt)
    nil))
