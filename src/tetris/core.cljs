(ns tetris.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan put! close! timeout alts!]]
            [tetris.helpers :as h]))

(def canvas (h/by-id "canvas"))
(def context (.getContext canvas "2d"))

(def WIDTH 300)
(def HEIGHT 300)

(set! (.-width canvas) WIDTH)
(set! (.-height canvas) HEIGHT)

(def block-size 10)

(defprotocol Moveable
  (move [this world dx dy])
  (can-move? [this world dx dy]))

(defprotocol Rotatable
  (rotate [this world]))

(defrecord Block [x y])
(defrecord Formation [loc orientations])
(defrecord World [curr-formation blocks])

(defn create-T-formation []
  (->Formation [27 10]
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

(defn get-block-at [world [x y]]
  (first (filter (fn [block] (and (= x (:x block))
                                  (= y (:y block))))
                 (:blocks world))))

(defn get-blocks-from-formation [formation]
  (filter (complement nil?) (flatten (first (:orientations formation)))))

(defn translated-blocks [formation]
  (let [[fx fy] (:loc formation)]
    (mapv (fn [{x :x y :y}] (->Block (+ fx x) (+ fy y))) (get-blocks-from-formation formation))))

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
              (assoc :curr-formation (create-T-formation)))
        world))))

(extend-type Formation
  Rotatable
  (rotate [formation world]
    (let [os (:orientations formation)]
      (assoc world :curr-formation (->Formation (:loc formation)
                                                (conj (vec (rest os))
                                                      (first os)))))))
(defn gen-world []
  (->World (create-T-formation)
           []))

(defn draw-block [{x :x y :y} ctx]
  (when-not (nil? x)
    (set! (.-fillStyle ctx) "rgb(0, 0, 0)")
    (.fillRect ctx (* x block-size) (* y block-size) block-size block-size)))

(def events (chan))

(go (while true
    (<! (timeout 700))
    (>! events :drop)))

(let [input (h/listen js/document :keydown)]
  (go
    (while true
      (let [e (<! input)]
        (condp = (.-keyIdentifier (.-event_ e))
          "Right" (>! events :right)
          "Left" (>! events :left)
          "Down" (>! events :drop)
          "Up" (>! events :rotate)
          nil)))))
(go
  (loop [world (gen-world)]
    (let [event (<! events)]
          (.clearRect context 0 0 WIDTH HEIGHT)
          (let [new-world (condp = event
                            :left (move (:curr-formation world) world -1 0)
                            :right (move (:curr-formation world) world 1 0)
                            :drop (move (:curr-formation world) world 0 1)
                            :rotate (rotate (:curr-formation world) world)
                           world)]
            (doseq [block (into (:blocks new-world)
                                (translated-blocks (:curr-formation new-world)))]
              (draw-block block context))
            (recur new-world)))))
