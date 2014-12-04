(ns clj-graphics2d.binary-morphology
  (:require [clj-graphics2d.util :refer [get2d assoc2d update2d
                                         updating-coll-by]])
  (:import java.awt.Rectangle
           (java.awt.image BufferedImage
                           BufferedImageOp)))

(defn run-length-encoding [& codes]
  (into []
   (loop [acc   []
          codes codes]
     (if codes
       (if (sequential? (first codes))
         (let [[num val] (first codes)]
           (recur (concat acc (take num (repeat val)))
                  (next codes)))
         (recur (concat acc [(first codes)])
                (next codes)))
       acc))))

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
    :dilate (fn [src _] src)
    :erode (fn [src _] src)
    :open (fn [src _] src)
    :close (fn [src _] src)
    identity))

(defn morphological-op [op-key]
  (let [op (get-morphological-op op-key)]
    (reify BufferedImageOp
      (filter [this src dst]
        (let [dst (or dst
                      (.createCompatibleDestImage this src nil))]
         (op src dst)))
      (createCompatibleDestImage [this src dest-cm]
        (let [dest-cm (or dest-cm
                          (.getColorModel src))
              w (.getWidth src)
              h (.getHeight src)]
          (BufferedImage. dest-cm
                          (.createCompatibleWritableRaster dest-cm w h)
                          (.isAlphaPremultiplied dest-cm)
                          nil)))
      (getRenderingHints [this]
        nil)
      (getBounds2D [this src]
        (Rectangle. 0 0 (.getWidth src) (.getHeight src)))
      (getPoint2D [this src-pt dst-pt]
        (.clone src-pt)))))
