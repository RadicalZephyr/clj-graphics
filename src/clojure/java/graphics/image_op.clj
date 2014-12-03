(ns clojure.java.graphics.image-op
  (:import (java.awt.color ColorSpace)
           (java.awt.geom AffineTransform)
           (java.awt.image AffineTransformOp ByteLookupTable
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
