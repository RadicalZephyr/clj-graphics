(ns clj-graphics2d.util
  (:require [clojure.java.io :as io])
  (:import (java.awt.image BufferedImage)
           (javax.imageio ImageIO)))

;; Utility macro for arbitrary cleanup from The Joy of Clojure

(defmacro with-cleanup [[binding value :as let-vec] close-fn & forms]
  `(let ~let-vec
     (try
       ~@forms
       (finally (~close-fn ~binding)))))

(defmacro ->interleave [x inter-fn & forms]
  `(let [~'wrapped-fn (fn [~'i]
                        ~(if (seq? inter-fn)
                           (with-meta `(~(first inter-fn) ~'i ~@(next inter-fn))
                             (meta inter-fn))
                           (list inter-fn 'i))
                        ~'i)]
     (-> ~x
         ~@(interleave (repeat 'wrapped-fn)
                      forms))))

;; Saving and opening image files

(defn file->image [filename]
  (ImageIO/read filename))

(defn image->file [img filename]
  (let [ext (last (clojure.string/split filename #"\."))]
    (ImageIO/write img ext (io/file filename))))


;; Pixel access

(defn get-pixels [img x y w h]
  (cond
   (or (= w 0)
       (= h 0)) []
   :else
   (let [img-type (.getType img)
         pixels (int-array (* w h))]
     (into []
           (if (or (= img-type BufferedImage/TYPE_INT_ARGB)
                   (= img-type BufferedImage/TYPE_INT_RGB))
             (let [raster (.getRaster img)]
               (.getDataElements raster x y w h pixels))
             (do
               (.getRGB img x y w h pixels 0 w)
               pixels))))))

(defn get-all-pixels [img]
  (let [w (.getWidth  img)
        h (.getHeight img)]
    (get-pixels img 0 0 w h)))

(defn all-translations [img [w h]]
  (let [rgbs (get-all-pixels img)
        img-w (.getWidth  img)
        img-h (.getHeight img)
        get-xy (fn [coll [x y]]
                 (get coll (+ (* y img-w)
                              x)))]
    (for [y (range (- img-h h))
          x (range (- img-w w))]
      {:point [x y]
       :pixels (map (partial get-xy rgbs)
                    (for [dx (range w)
                          dy (range h)]
                      [(+ x dx) (+ y dy)]))})))

(defn set-pixels [img x y w h pixels]
  (cond
   (or (not pixels)
       (= w 0)
       (= h 0)) nil
   (> (* w h)
      (count pixels))
   (throw (ex-info "Pixels length must be greater than w*h"
                   {:count (count pixels)
                    :width w
                    :height h}))
   :else (let [img-type (.getType img)
               pixels (int-array pixels)]
           (if (or (= img-type BufferedImage/TYPE_INT_ARGB)
                   (= img-type BufferedImage/TYPE_INT_RGB))
             (let [raster (.getRaster img)]
               (.setDataElements raster x y w h pixels))
             (.setRGB img x y w h pixels 0 w))
           img)))
