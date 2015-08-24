(ns clj-graphics2d.adjacent
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(defn basic-adjacencies [dimensions]
  (m/sparse-matrix
   (m/emap #(if (= 1.0 %)
              (- %) %)
           (m/mutable
            (m/identity-matrix dimensions)))))

(defn adj-dispatcher [adj _ _]
  (cond (m/matrix? adj) :matrix
        (map? adj)      :map))

(defmulti adjacent?
  "Predicate to determine if two labels are adjacent."
  adj-dispatcher)

(defmulti set-adjacent
  "Set two labels to be adjacent."
  adj-dispatcher)

(defmethod adjacent?    :matrix [adjacencies l1 l2]
  (try
    (= 1.0
       (m/mget adjacencies l1 l2)
       (m/mget adjacencies l2 l1))
    (catch java.lang.IndexOutOfBoundsException ex
      (throw (ex-info "Unexpected label value"
                      {:label-1 l1 :lablel-2 l2}
                      ex)))))

(defmethod set-adjacent :matrix [adjacencies l1 l2]
  (try
    (-> adjacencies
        (m/mset l1 l2 1.0)
        (m/mset l2 l1 1.0))
    (catch java.lang.ArrayIndexOutOfBoundsException ex
      (throw (ex-info "Unexpected label value"
                      {:label-1 l1 :lablel-2 l2}
                      ex)))))

(defmethod adjacent?    :map [adjacencies l1 l2]
  (boolean
   (and (some #{l2} (get adjacencies l1 []))
        (some #{l1} (get adjacencies l2 [])))))

(defmethod set-adjacent :map [adjacencies l1 l2]
  (let [append-label #(conj (vec %1) %2)]
    (-> adjacencies
        (update-in [l1] append-label l2)
        (update-in [l2] append-label l1))))

(defn update-adjacencies [adjacencies zero-count current-label next-label]
  (let [next-label (int next-label)]
    (dosync
     (if (not= next-label 0)
       (do
         (let [current-label @current-label]
           (when (and (not= current-label next-label)
                      (not= current-label -1)
                      (>= 1 @zero-count))
             (alter adjacencies
                    set-adjacent current-label next-label)))
         (ref-set current-label next-label)
         (ref-set zero-count 0))
       (alter zero-count inc)))))

(defn make-update-adjacencies [& {:keys [initial-label dimensions]}]
  (let [adjacencies (ref (basic-adjacencies dimensions))
        zero-count  (ref 0)
        current-label (ref initial-label)]
    {:adjacencies adjacencies
     :zero-count zero-count
     :current-label current-label
     :reset-state! (fn [] (dosync (ref-set current-label -1)))
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

        {:keys [adjacencies zero-count current-label
                do-update reset-state!]}
        (make-update-adjacencies :initial-label -1
                                 :dimensions num-labels)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (do-update (m/mget pixels y x)))
      (reset-state!))
    (reset-state!)
    (doseq [x (range width)]
      (doseq [y (range height)]
        (do-update (m/mget pixels y x)))
      (reset-state!))
    @adjacencies))
