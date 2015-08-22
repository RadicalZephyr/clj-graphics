(ns clj-graphics2d.image-op
  (:require [clj-graphics2d.binary-morphology :as bm]
            [clj-graphics2d.util :refer [get-all-pixels-binary
                                         set-all-pixels-binary]])
  (:import (java.awt Rectangle)
           (java.awt.color ColorSpace)
           (java.awt.geom AffineTransform)
           (java.awt.image AffineTransformOp
                           BufferedImage
                           BufferedImageOp
                           ByteLookupTable
                           ColorConvertOp LookupOp)))

(defn threshold-table [threshold]
  (byte-array (map (fn [x] (if (< x threshold) 0 255))
                   (range 256))))

(def invert-table
  (byte-array (map (fn [x] (- 255 x))
                   (range 256))))

(defn make-scale-op [scale]
  (let [scale (float scale)
        aft (AffineTransform.)]
    (.scale aft scale scale)
    (AffineTransformOp. aft AffineTransformOp/TYPE_BILINEAR)))

(defn make-lookup-op [table]
  (LookupOp. (ByteLookupTable. 0 table) nil))

(defn scale-image [img scale]
  (let [scale-op (make-scale-op scale)]
    (.filter scale-op img nil)))

(defn invert-image [img]
  (let [lookup-op (make-lookup-op invert-table)]
    (.filter lookup-op img nil)))

(defn threshold-image [img threshold]
  (let [threshold-op (make-lookup-op (threshold-table threshold))]
    (.filter threshold-op img nil)))

(defn color-convert-image [img colorspace]
  (let [ccop (ColorConvertOp. (ColorSpace/getInstance
                               colorspace)
                              nil)]
    (.filter ccop img nil)))

(defn morphological-op [op]
  (reify BufferedImageOp
    (filter [this src dst]
      (let [dst (or dst
                    (.createCompatibleDestImage this src nil))
            pixels (get-all-pixels-binary src)
            w (.getWidth  src)
            h (.getHeight src)]
        (->> (op (bm/binary-image [w h] pixels))
             bm/image
             (set-all-pixels-binary dst))))
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
      (.clone src-pt))))

(defn morpho-image [img op]
  (let [morpho-op (morphological-op op)]
    (.filter morpho-op img nil)))
