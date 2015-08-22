(ns clj-graphics2d.display
  (:require [clj-graphics2d.connect :as cn]
            [clj-graphics2d.corner :as corner]
            [clj-graphics2d.binary-morphology :as bm]
            [clj-graphics2d.image-op :as op]
            [clj-graphics2d.util :as util]
            [clojure.java.io :as io]
            [seesaw.core :as s])
  (:import (java.awt Color)
           (java.awt.color ColorSpace)))

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
                     :minimum-size [760 :by 780]
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

(defn remove-header [img]
  (util/with-cleanup [gfx (.createGraphics img)] .dispose
    (.setBackground gfx Color/WHITE)
    (.clearRect gfx 396 3 357 70)
    (.clearRect gfx 394 3 1 70))
  img)

(defn draw-and-sleep [img time]
  (draw-image img (get-canvas))
  (Thread/sleep time))

(defn img->bimg [img]
  (bm/binary-image [(.getWidth  img)
                    (.getHeight img)]
                   (util/get-all-pixels-binary img)))

(defn process-image [img]
  (let [iimg (util/->interleave img (draw-image (get-canvas))
                                remove-header
                                (op/color-convert-image ColorSpace/CS_GRAY)
                                op/invert-image
                                (op/threshold-image 150))

        components-seq    (map second (drop 1 (components iimg)))
        grouped           (group-by #(> (count %) 100)
                                    components-seq)
        border-component  (first (grouped true))
        words-component   (partition 2 (flatten (grouped false)))]
    (let [corner-image
          (util/->interleave iimg (draw-image (get-canvas))
                             (op/color-convert-image ColorSpace/CS_sRGB)
                             (color-component words-component Color/BLACK)
                             (op/color-convert-image ColorSpace/CS_GRAY)
                             op/invert-image
                             (op/threshold-image 150))
          corners (corner/get-all-corners
                   (util/all-translations corner-image [3 3]))]
      {:img corner-image
       :border-component border-component
       :words-component words-component
       :corners corners})))

(defn do-processing []
  (-main)
  (.setAlwaysOnTop @root true)
  (def data (-> (io/file "resources" "diplo-map-simple.gif")
                util/file->image
                process-image)))
