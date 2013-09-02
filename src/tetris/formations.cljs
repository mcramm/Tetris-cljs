(ns tetris.formations
  (:require [tetris.helpers :as h]
            [tetris.world :refer [block-size WIDTH HEIGHT get-block-at]]))

(defrecord Block [x y color])
(defrecord Formation [loc color orientations])

(defprotocol Moveable
  (move [this world dx dy])
  (can-move? [this world dx dy]))

(defprotocol Rotatable
  (rotate [this world]))


(defn- get-blocks-from-formation [formation]
  (filter (complement nil?) (flatten (first (:orientations formation)))))


(defn translated-blocks [formation]
  (let [[fx fy] (:loc formation)]
    (mapv (fn [{x :x y :y}] (->Block (+ fx x) (+ fy y) (:color formation))) (get-blocks-from-formation formation))))

(def start-x (/ (Math/floor (/ WIDTH 2)) block-size))
(def start-y -2)
(defn create-block [x y color]
  (->Block x y color))

(def T-Formation
  [[(->Block 0 0 nil)
    (->Block 1 0 nil)
    (->Block 2 0 nil)
    (->Block 1 1 nil)]
   [(->Block 1 0 nil)
    (->Block 1 1 nil)
    (->Block 1 2 nil)
    (->Block 0 1 nil)]
   [(->Block 0 1 nil)
    (->Block 1 1 nil)
    (->Block 2 1 nil)
    (->Block 1 0 nil)]
   [(->Block 0 0 nil)
    (->Block 0 1 nil)
    (->Block 0 2 nil)
    (->Block 1 1 nil)]])

(def I-Formation
  [[(->Block 0 0 nil)
    (->Block 0 1 nil)
    (->Block 0 2 nil)
    (->Block 0 3 nil)]
   [(->Block 0 0 nil)
    (->Block 1 0 nil)
    (->Block 2 0 nil)
    (->Block 3 0 nil)]])

(def Z-Formation
  [[(->Block 0 0 nil)
    (->Block 1 0 nil)
    (->Block 1 1 nil)
    (->Block 2 1 nil)]
   [(->Block 1 0 nil)
    (->Block 0 1 nil)
    (->Block 1 1 nil)
    (->Block 0 2 nil)]])

(def RZ-Formation
  [[(->Block 0 1 nil)
    (->Block 1 1 nil)
    (->Block 1 0 nil)
    (->Block 2 0 nil)]
   [(->Block 0 0 nil)
    (->Block 0 1 nil)
    (->Block 1 1 nil)
    (->Block 1 2 nil)]])

(def S-Formation
  [[(->Block 0 0 nil)
    (->Block 0 1 nil)
    (->Block 1 0 nil)
    (->Block 1 1 nil)]])

(def L-Formation
  [[(->Block 0 0 nil)
    (->Block 0 1 nil)
    (->Block 0 2 nil)
    (->Block 1 2 nil)]
   [(->Block 0 1 nil)
    (->Block 1 1 nil)
    (->Block 2 1 nil)
    (->Block 2 0 nil)]
   [(->Block 0 0 nil)
    (->Block 1 0 nil)
    (->Block 1 1 nil)
    (->Block 1 2 nil)]
   [(->Block 0 0 nil)
    (->Block 1 0 nil)
    (->Block 2 0 nil)
    (->Block 0 1 nil)]])

(def RL-Formation
  [[(->Block 1 0 nil)
    (->Block 1 1 nil)
    (->Block 1 2 nil)
    (->Block 0 2 nil)]
   [(->Block 0 0 nil)
    (->Block 1 0 nil)
    (->Block 2 0 nil)
    (->Block 2 1 nil)]
   [(->Block 0 0 nil)
    (->Block 0 1 nil)
    (->Block 0 2 nil)
    (->Block 1 0 nil)]
   [(->Block 0 0 nil)
    (->Block 0 1 nil)
    (->Block 1 1 nil)
    (->Block 2 1 nil)]])

(defn create-formation [blocks color]
  (let [blocks (mapv (fn [o] (mapv (fn [b] (assoc b :color color)) o)) blocks)]
    (->Formation [start-x start-y] color blocks)))
      ; (assoc :blocks (apply mapv (fn [b] (assoc b :color color)) (:blocks

(def colors ["rgb(255,0,0)", "rgb(0,255,0)", "rgb(0,0,255)"])

(def formations [T-Formation I-Formation Z-Formation RZ-Formation S-Formation L-Formation RL-Formation])

(defn random-formation []
  (create-formation (rand-nth formations) (rand-nth colors)))

(extend-type Formation
  Moveable
  (can-move? [formation world dx dy]
    (let [[fx fy] (:loc formation)]
      (and (every? nil? (flatten (mapv (fn [b] (get-block-at world [(+ (:x b) dx) (+ (:y b) dy)])) (translated-blocks formation))))
           (every? true? (flatten (mapv (fn [{x :x y :y}]
                                          (and (< (* (+ dy y) block-size) HEIGHT)
                                               (>= (* (+ dx x) block-size) 0)
                                               (< (* (+ dx x) block-size) WIDTH)
                                               ))
                   (translated-blocks formation)))))))
  (move [formation world dx dy]
    (if (can-move? formation world dx dy)
      (let [[fx fy] (:loc formation)]
        (assoc world :curr-formation (->Formation [(+ fx dx) (+ fy dy)] (:color formation) (:orientations formation))))
      (if (> dy 0)
          (-> world
              (assoc :blocks (into (:blocks world) (translated-blocks formation)))
              (assoc :curr-formation (random-formation)))
        world))))

(extend-type Formation
  Rotatable
  (rotate [formation world]
    (let [os (:orientations formation)]
      (assoc world :curr-formation (->Formation (:loc formation)
                                                (:color formation)
                                                (conj (vec (rest os))
                                                      (first os)))))))
