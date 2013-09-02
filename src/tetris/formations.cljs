(ns tetris.formations
  (:require [tetris.helpers :as h]
            [tetris.world :refer [block-size WIDTH HEIGHT get-block-at]]))

(defrecord Block [x y])
(defrecord Formation [loc orientations])

(defprotocol Moveable
  (move [this world dx dy])
  (can-move? [this world dx dy]))

(defprotocol Rotatable
  (rotate [this world]))


(defn- get-blocks-from-formation [formation]
  (filter (complement nil?) (flatten (first (:orientations formation)))))


(defn translated-blocks [formation]
  (let [[fx fy] (:loc formation)]
    (mapv (fn [{x :x y :y}] (->Block (+ fx x) (+ fy y))) (get-blocks-from-formation formation))))

(def start-x (/ (Math/floor (/ WIDTH 2)) block-size))
(def start-y -2)
(defn create-block [x y]
  (->Block x y))

(defn create-T []
  (->Formation [start-x start-y]
                   [[(->Block 0 0)
                     (->Block 1 0)
                     (->Block 2 0)
                     (->Block 1 1)]
                    [(->Block 1 0)
                     (->Block 1 1)
                     (->Block 1 2)
                     (->Block 0 1)]
                    [(->Block 0 1)
                     (->Block 1 1)
                     (->Block 2 1)
                     (->Block 1 0)]
                    [(->Block 0 0)
                     (->Block 0 1)
                     (->Block 0 2)
                     (->Block 1 1)]]))

(defn create-I []
  (->Formation [start-x start-y]
                   [[(->Block 0 0)
                     (->Block 0 1)
                     (->Block 0 2)
                     (->Block 0 3)]
                    [(->Block 0 0)
                     (->Block 1 0)
                     (->Block 2 0)
                     (->Block 3 0)]]))

(defn create-Z []
  (->Formation [start-x start-y]
               [[(->Block 0 0)
                 (->Block 1 0)
                 (->Block 1 1)
                 (->Block 2 1)]
                [(->Block 1 0)
                 (->Block 0 1)
                 (->Block 1 1)
                 (->Block 0 2)]]))

(defn create-RZ []
  (->Formation [start-x start-y]
               [[(->Block 0 1)
                 (->Block 1 1)
                 (->Block 1 0)
                 (->Block 2 0)]
                [(->Block 0 0)
                 (->Block 0 1)
                 (->Block 1 1)
                 (->Block 1 2)]]))

(defn create-S []
  (->Formation [start-x start-y]
               [[(->Block 0 0)
                 (->Block 0 1)
                 (->Block 1 0)
                 (->Block 1 1)]]))

(defn create-L []
  (->Formation [start-x start-y]
               [[(->Block 0 0)
                 (->Block 0 1)
                 (->Block 0 2)
                 (->Block 1 2)]
                [(->Block 0 1)
                 (->Block 1 1)
                 (->Block 2 1)
                 (->Block 2 0)]
                [(->Block 0 0)
                 (->Block 1 0)
                 (->Block 1 1)
                 (->Block 1 2)]
                [(->Block 0 0)
                 (->Block 1 0)
                 (->Block 2 0)
                 (->Block 0 1)]]))
(defn create-RL []
  (->Formation [start-x start-y]
               [[(->Block 1 0)
                 (->Block 1 1)
                 (->Block 1 2)
                 (->Block 0 2)]
                [(->Block 0 0)
                 (->Block 1 0)
                 (->Block 2 0)
                 (->Block 2 1)]
                [(->Block 0 0)
                 (->Block 0 1)
                 (->Block 0 2)
                 (->Block 1 0)]
                [(->Block 0 0)
                 (->Block 0 1)
                 (->Block 1 1)
                 (->Block 2 1)]]))

(def formations [create-T create-I create-Z create-RZ create-S create-L create-RL])

(defn random-formation []
  ((rand-nth formations)))

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
        (assoc world :curr-formation (->Formation [(+ fx dx) (+ fy dy)] (:orientations formation))))
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
                                                (conj (vec (rest os))
                                                      (first os)))))))
