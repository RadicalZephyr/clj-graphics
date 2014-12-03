(ns clojure.java.graphics.binary-morphology
  (:require [clojure.java.graphics.util :as util])
  (:import java.awt.Rectangle
           (java.awt.image BufferedImage
                           BufferedImageOp)))

(defn get-kernel-from-st-el [[x y] {[ox oy] :origin
                                  [w h] :dimensions}]
  (for [dy (range h)
        dx (range w)]
    [(+ x (- dx ox)) (+ y (- dy oy))]))

(defn is-in-bounds? [[w h] [x y]]
  (and (> w x -1)
       (> h y -1)))

(defn all-translations [img st-el]
  (let [img-w (.getWidth  img)
        img-h (.getHeight img)
        img-dim [img-w img-h]
        rgbs (util/get-pixels img 0 0 img-w img-h)
        get-xy (fn [coll [x y]]
                 (get coll (+ (* y img-w)
                              x)))]
    (for [y (range img-h)
          x (range img-w)
          :let [kernel (->>
                        (get-kernel-from-st-el [x y] st-el)
                        (filter (partial is-in-bounds? img-dim))
                        (map (partial get-xy rgbs)))]]
      kernel)))


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
