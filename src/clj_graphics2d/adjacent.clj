(ns clj-graphics2d.adjacent
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(defn basic-adjacencies [dimensions]
  (m/emap #(if (= 1.0 %)
             (- %) %)
          (m/mutable (m/identity-matrix dimensions))))

(defn update-adjacencies [adjacencies zero-count current-label next-label]
  (dosync
   (if (not= next-label 0)
     (do
       (let [cl @current-label]
         (when (and (not= cl next-label)
                    (not= cl -1)
                    (>= 1 @zero-count))
           (alter adjacencies
                  #(-> %
                       (m/mset cl next-label 1.0)
                       (m/mset next-label cl 1.0)))))
       (ref-set current-label next-label)
       (ref-set zero-count 0))
     (alter zero-count inc))))

(defn make-update-adjacencies [& {:keys [initial-label dimensions]}]
  (let [adjacencies (ref (basic-adjacencies dimensions))
        zero-count  (ref 0)
        current-label (ref initial-label)]
    {:adjacencies adjacencies
     :zero-count zero-count
     :current-label current-label
     :do-update
     (fn [next-label]
       (update-adjacencies adjacencies zero-count
                           current-label next-label))}))

(defn get-unique-labels [pixels]
  (m/ereduce #(conj %1 (int %2))
             #{0} pixels))

(defn adjacencies [pixels]
  (let [unique-labels (get-unique-labels pixels)
        num-labels    (count unique-labels)
        [height width] (m/shape pixels)
        adjacencies (basic-adjacencies num-labels)]
    adjacencies))
