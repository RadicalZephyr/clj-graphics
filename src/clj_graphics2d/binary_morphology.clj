(ns clj-graphics2d.binary-morphology
  (:require [clj-graphics2d.util :as util])
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

(defn get-kernel-from-st-el [[x y] {[ox oy] :origin
                                  [w h] :dimensions}]
  (for [dy (range h)
        dx (range w)]
    [(+ x (- dx ox)) (+ y (- dy oy))]))

(defn is-in-bounds? [[w h] [x y]]
  (and (> w x -1)
       (> h y -1)))

(defn acc-1s [bimg acc [[x y :as pt] el]]
  (if (is-in-bounds? [8 8] pt)
    (let [val (bit-or (get bimg (+ (* y 8)
                                   x))
                      el)]
      (update-in acc [val] concat [pt]))
    acc))

(defn all-translations [[w h] st-el]
  (for [y (range h)
        x (range w)]
    (get-kernel-from-st-el [x y] st-el)))


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
