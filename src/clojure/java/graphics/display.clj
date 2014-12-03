(ns clojure.java.graphics.display
  (:require [clojure.java.graphics.connect :as cn]
            [clojure.java.graphics.corner :as corner]
            [clojure.java.graphics.util :as util]
            [clojure.java.io :as io]
            [seesaw.core :as s]
            [seesaw.graphics :as g])
  (:import  java.awt.Color
            java.awt.color.ColorSpace
            java.awt.geom.AffineTransform
            (java.awt.image AffineTransformOp
                            BufferedImage
                            ByteLookupTable
                            ColorConvertOp
                            LookupOp
                            PixelGrabber)))

;; Seesaw related infrastructure

(def root (atom nil))

(defn draw-image [buffered-image canvas & {:keys [op]}]
  (s/config! canvas :paint
             (fn [c g]
               (.drawImage g buffered-image op 0 0))))

(defn draw-image-rect [img canvas rect]
  (s/config! canvas :paint
             (fn [c g]
               (.drawImage g img nil 0 0)
               (.fill g rect))))

(defn get-canvas []
  (s/select @root [:#canvas]))

(defn show-frame [frame]
  (s/invoke-later
   (-> frame
       s/pack!
       s/show!)))

(defn -main [& args]
  (compare-and-set!
   root nil (s/frame :title "Images!!"
                     :minimum-size [385 :by 410]
                     :content (s/canvas :id :canvas)))
  (show-frame @root))


;; Process the image as a vector of integers

(defn get-area-subimage [img points]
  (let [xs (map first points)
        ys (map second points)
        x-min (apply min xs)
        x-max (apply max xs)
        y-min (apply min ys)
        y-max (apply max ys)]
    (when (and (> x-max x-min)
               (> y-max y-min))
      (.getSubimage img x-min y-min (- x-max x-min) (- y-max y-min)))))


;; Connected components

(defn rgb->image [img rgb]
  (let [img-w (.getWidth  img)
        img-h (.getHeight img)
        nimg (BufferedImage. img-w img-h BufferedImage/TYPE_INT_RGB)]
    (dorun
     (for [x (range img-w)
           y (range img-h)
           :let [c (Color. (cn/get2d rgb [x y]))]]
       (.setRGB img x y c)))
    nimg))

(defn connected-image [img]
  (let [rgb (util/get-all-pixels  img)
        img-w (.getWidth  img)
        img-h (.getHeight img)
        nrgb (->> rgb
                  (replace {-16777216 0})
                  (cn/connected-components img-w img-h))]
    (util/set-pixels img nrgb)))

(defn components [img]
  (let [rgb (util/get-all-pixels img)
        w (.getWidth  img)
        h (.getHeight img)
        crgb (->> rgb
                  (replace {-16777216 0})
                  (cn/connected-components w h))
        pts (for [x (range w)
                  y (range h)]
              [x y])]
    (binding [cn/max-x w cn/max-y h]
      (->> pts
           (reduce
            (fn [acc pt]
              (update-in acc [(cn/get2d crgb pt)]
                         conj pt))
            {})))))


;; Coloring components

(def color-cycle (cycle [Color/GREEN Color/BLUE Color/MAGENTA
                         Color/CYAN  Color/PINK Color/ORANGE]))

(defn color-component [img pts color]
  (doseq [[x y] pts]
    (.setRGB img x y (.getRGB color)))
  img)

(defn color-components [img components colors]
  (dorun
   (map (partial color-component img) components colors)))


;; Image pre-processing stuff

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

(do
  (def f (io/file "resources" "diplo-map-simple.gif"))
  (def img  (util/file->image f))
  (util/with-cleanup [gfx (.createGraphics img)] .dispose
    (.setBackground gfx Color/WHITE)
    (.clearRect gfx 396 3 357 70)
    (.clearRect gfx 394 3 1 70))
  (-main)
  (def iimg (-> img
                (color-convert-image ColorSpace/CS_GRAY)
                invert-image
                (threshold-image 150)))
  (def components-seq (map second (drop 1 (components iimg))))
  (let [grouped (group-by #(> (count %) 100)
                   components-seq)]
    (def border-component (first (grouped true)))
    (def words-component (partition 2 (flatten (grouped false)))))

  (def corner-img (-> iimg
                      (color-convert-image ColorSpace/CS_sRGB)
                      (color-component words-component Color/BLACK)
                      (color-convert-image ColorSpace/CS_GRAY)
                      invert-image
                      (threshold-image 150)))
   ;; Get the only element of the list
  (draw-image corner-img
              (get-canvas))

  (def corners (corner/get-all-corners
                (util/all-translations img [3 3]))))
