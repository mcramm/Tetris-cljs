(ns tetris.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan put! close! timeout alts!]]
            [tetris.helpers :as h]
            [tetris.formations :as formations]
            [tetris.world :refer [block-size WIDTH HEIGHT]]))

(def canvas (h/by-id "canvas"))
(def context (.getContext canvas "2d"))
(set! (.-width canvas) WIDTH)
(set! (.-height canvas) HEIGHT)

(defrecord World [curr-formation blocks])

(defn gen-world []
  (->World (formations/random-formation)
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
                            :left (formations/move (:curr-formation world) world -1 0)
                            :right (formations/move (:curr-formation world) world 1 0)
                            :drop (formations/move (:curr-formation world) world 0 1)
                            :rotate (formations/rotate (:curr-formation world) world)
                           world)]
            (doseq [block (into (:blocks new-world)
                                (formations/translated-blocks (:curr-formation new-world)))]
              (draw-block block context))
            (recur new-world)))))
