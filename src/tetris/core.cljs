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

(defrecord Block [x y])
(defrecord Formation [blocks])
(defrecord World [curr-formation blocks])

(defn get-block-at [blocks [x y]]
  (first (filter (fn [block] (and (= x (:x block))
                                  (= y (:y block))))
                 blocks)))

(defn get-block-below [world {x :x y :y}]
  (get-block-at (:blocks world) [x (inc y)]))

(defn new-formation []
  (->Formation [(->Block 5 5) (->Block 6 6) (->Block 5 6) (->Block 5 7)]))

(defn empty-blocks []
  (vec (repeatedly 4 (fn [] (vec (repeat 4 nil))))))

(defn create-L-formation []
  (-> (->Formation (empty-blocks))
      (assoc-in [:blocks 0 0] (->Block 0 0))
      (assoc-in [:blocks 1 0] (->Block 1 0))
      (assoc-in [:blocks 2 0] (->Block 2 0))
      (assoc-in [:blocks 3 0] (->Block 3 0))))

(defn create-T-formation []
  (-> (->Formation (empty-blocks))
      (assoc-in [:blocks 0 0] (->Block 3 0))
      (assoc-in [:blocks 0 1] (->Block 4 0))
      (assoc-in [:blocks 0 3] (->Block 5 0))
      (assoc-in [:blocks 1 1] (->Block 4 1))))

(extend-type Formation
  Moveable
  (can-move? [formation world dx dy]
    (and (every? nil? (flatten (map (fn [row] (map (fn [b] (get-block-at (:blocks world) [(+ (:x b) dx) (+ (:y b) dy)])) row)) (:blocks formation))))
         (every? true? (flatten (map (fn [row] (map (fn [{x :x y :y}] 
                                                       (and (< (* (+ dy y) block-size) HEIGHT)
                                                             (>= (* (+ dx x) block-size) 0)
                                                             (< (* (+ dx x) block-size) WIDTH)
                                                            )) (filter #(not (nil? %)) row)))
                                      (:blocks formation))))))

  (move [formation world dx dy]
    (if (can-move? formation world dx dy)
      (assoc world :curr-formation (->Formation (map (fn [row]
                                                       (map (fn [{x :x y :y}]
                                                              (if (nil? x)
                                                                nil
                                                                (->Block (+ dx x) (+ dy y))))
                                                       row))
                                                (:blocks formation))))
      (if (> dy 0)
        (-> world
            (assoc :blocks (into (:blocks world) (flatten (:blocks formation))))
            (assoc :curr-formation (create-T-formation)))
        world))))

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
                           world)]
            (doseq [block (into (:blocks new-world)
                                (flatten (get-in new-world [:curr-formation :blocks])))]
              (draw-block block context))
            (recur new-world)))))
