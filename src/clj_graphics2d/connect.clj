(ns clj-graphics2d.connect
  (:require [clj-graphics2d.union-find :as uf]
            [clj-graphics2d.util :refer [updating-coll-by]]))

(def ^:dynamic max-x)

(def ^:dynamic max-y)

(defn get2d [coll [x y]]
  (get coll (+ (* y max-x)
               x)))

(defn assoc2d [coll [x y] val]
  (assoc coll
    (+ (* y max-x)
       x)
    val))

(defn bounded-neighbours [[x y]]
  (doall
   (for [dx [-1 0 1]
         dy [-1 0 1]
         :let [nx (+ dx x)
               ny (+ dy y)]
         :when (and (or (= dx 0)
                        (= dy 0))
                    (not= 0 dx dy)
                    (> max-x nx -1)
                    (> max-y ny -1))]
     [(+ dx x) (+ dy y)])))

(defn search [rgbs label pt]
  (updating-coll-by [rgbs (assoc2d rgbs pt label)
                          pts (bounded-neighbours pt)
                          :head-as pt
                          :when (= (get2d rgbs pt) -1)]
    (search rgbs label pt)))

(defn find-components [rgbs label]
  (updating-coll-by [rgbs rgbs
                     pts  (for [x (range max-x)
                                y (range max-y)]
                            [x y])
                     :head-as pt
                     :when (= (get2d rgbs pt)
                              -1)]
    (do (swap! label inc)
        (search rgbs @label pt))))

(defn recursive-connected-components [rgbs]
  (let [label (atom 0)]
    (find-components rgbs label)))

(defn prior-neighbours [[x y]]
  (doall
   (for [dx [-1 0]
         dy [-1 0]
         :let [nx (+ dx x)
               ny (+ dy y)]
         :when (and (not= dx dy)
                    (> max-x nx -1)
                    (> max-y ny -1))]
     [nx ny])))

(defn labels [rgbs pts]
  (->> pts
       (map (partial get2d rgbs))
       (filter #(not= 0 %))))

(defn pt->pt-and-pn [pts]
  (map (fn [pt]
         {:pt pt
          :pn (prior-neighbours pt)})
       pts))

(defn first-pass [rgbs]
  (let [rgbs (atom rgbs)
        label (atom 0)
        unions (atom uf/empty-union-find)
        next-label!   (fn [] (swap! label inc))
        mark-pt!      (fn [pt m] (swap! rgbs assoc2d pt m))
        union-labels! (fn [min-label label]
                        (swap! unions uf/union label min-label))]

    (dorun
     (->> (for [y (range max-y)
                x (range max-x)] [x y])
          pt->pt-and-pn
          (map #(assoc % :value (get2d @rgbs (:pt %))))

          (map (fn [{:keys [pt pn value]}]
                 (when (= -1 value)
                   (if-let [labels (seq (labels @rgbs pn))]
                     (let [m (apply min labels)]
                       (dorun
                        (map (partial union-labels! m)
                             labels))
                       (mark-pt! pt m))
                     (do
                       (next-label!)
                       (mark-pt! pt @label))))))))

    [@rgbs @unions]))

(defn minimum-label-replacements [unions]
  (let [label (atom -1)]
    (->> unions
         count
         range
         (group-by (fn [label]
                     (uf/find unions label)))
         seq
         (mapcat (fn [[k vs]]
                   (let [min-label (swap! label inc)]
                     (map (fn [v]
                            [v min-label])
                          vs))))
         (into {}))))

(defn second-pass [[rgbs unions]]
  (let [equivalence-maps (minimum-label-replacements unions)]
    (replace equivalence-maps rgbs)))

(defn classical-connected-components [rgbs]
  (-> rgbs
      first-pass
      second-pass))

(defn connected-components [w h rgbs & {:keys [impl]
                                        :or   {impl :classical}}]
  (binding [max-x w
            max-y h]
    (case impl
      :recursive (recursive-connected-components rgbs)
      :classical (classical-connected-components rgbs)
      (throw (ex-info "No implementation of that type defined." {:impl impl})))))

(defn print-grid [grid w]
  (dorun (map println (partition w grid))))
