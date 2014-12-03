(ns clojure.java.graphics.corner)

(def corner-patterns-2x {[-16777216 -16777216 -16777216 -1] :tlo
                         [-16777216 -16777216 -1 -16777216] :tro
                         [-16777216 -1 -16777216 -16777216] :blo
                         [-1 -16777216 -16777216 -16777216] :bro
                         [-16777216 -1 -1 -1] :tli
                         [-1 -16777216 -1 -1] :tri
                         [-1 -1 -16777216 -1] :bli
                         [-1 -1 -1 -16777216] :bri})

(def corner-patterns-3x {[-16777216 -16777216 -16777216
                          -16777216 -1 -1
                          -16777216 -1 -1] :tlo
                         [-16777216 -16777216 -16777216
                          -1 -1 -16777216
                          -1 -1 -16777216] :tro
                         [-16777216 -1 -1
                          -16777216 -1 -1
                          -16777216 -16777216 -16777216] :blo
                         [-1 -1 -16777216
                          -1 -1 -16777216
                          -16777216 -16777216 -16777216] :bro
                         [-16777216 -1 -1
                          -1 -1 -1
                          -1 -1 -1] :tli
                         [-1 -1 -16777216
                          -1 -1 -1
                          -1 -1 -1] :tri
                         [-1 -1 -1
                          -1 -1 -1
                          -16777216 -1 -1] :bli
                         [-1 -1 -1
                          -1 -1 -1
                          -1 -1 -16777216] :bri})

(defn corner->inner-point
  "The inner point is defined as the point that is white-pixeled and
  most in the corner.

  That is for this pattern:
  x x
  x o

  where x's are black and o's are white, the \"o\" is the \"inner point\"."
  [{:keys [type point]}]
  (apply
   (case type
     :tlo (fn [x y] [(inc x) (inc y)])
     :tro (fn [x y] [(dec x) (inc y)])
     :blo (fn [x y] [(inc x) (dec y)])
     :bro (fn [x y] [(dec x) (dec y)])
     (:tli
      :tri
      :bli
      :bri) (fn [x y] [(inc x) (inc y)]))
   point))

(defn corner->outer-point
  "The outer point is definied as the point that is black-pixeled and
  in the actual corner.

  That is, for this pattern:
  X x
  x o

  Where x's are black and o's are white, the \"X\" is the \"outer point\"."
  [{:keys [type point]}]
  (apply
   (case type
     :tlo (fn [x y] [x y])
     :tro (fn [x y] [(+ 2 x) y])
     :blo (fn [x y] [x (+ 2 y)])
     :bro (fn [x y] [(+ 2 x) (+ 2 y)])
     :tli (fn [x y] [x y])
     :tri (fn [x y] [(+ 2 x) y])
     :bli (fn [x y] [x (+ 2 y)])
     :bri (fn [x y] [(+ 2 x) (+ 2 y)]))
   point))

(defn corner? [{pxs :pixels}]
  (corner-patterns-3x pxs))

(defn to-corner [{:keys [pixels] :as m}]
  (-> m
      (dissoc :pixels)
      (assoc :type (corner-patterns-3x pixels))))

(defn get-all-corners [grids]
  (->> grids
       (filter corner?)
       (map to-corner)))

(defn canonicalize-corner [corner]
  (-> corner
      (dissoc :point)
      (assoc :inner-point (corner->inner-point corner)
             :outer-point (corner->outer-point corner))))
