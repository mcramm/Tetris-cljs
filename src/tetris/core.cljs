(ns tetris.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan put! close! timeout alts!]]))

(defn by-id [id]
  (.getElementById js/document id))

(defn log [& args]
  (.log js/console (clj->js args)))

(def canvas (by-id "canvas"))
(def context (.getContext canvas "2d"))

(def WIDTH 300)
(def HEIGHT 300)

(set! (.-width canvas) WIDTH)
(set! (.-height canvas) HEIGHT)

(def block-size 10)

(defprotocol Moveable
  (move [this world dx dy])
  (can-move? [this world dx dy]))

(defprotocol Dropable
  (fall [this world])
  (can-fall? [this world]))

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

(extend-type Formation
  Dropable
  (can-fall? [formation world]
    (and (every? nil? (map #(get-block-below world %) (:blocks formation)))
         (every? false? (map (fn [{y :y}] 
                               (let [ny (inc y)]
                                 (>= (* ny block-size) HEIGHT))) (:blocks formation)))))
  (fall [formation world]
    (if (can-fall? formation world)
      (assoc world :curr-formation (->Formation (map (fn [{x :x y :y}]
                                                       (->Block x (inc y)))
                                                     (:blocks formation))))
      (-> world
          (assoc :blocks (into (:blocks world) (:blocks formation)))
          (assoc :curr-formation (new-formation))))))

; (extend-type Formation
;   Moveable
;   (can-move? [formation world nx ny]
;     (let [thing (filter (fn [b]
;                           (= (+ (* ny block-size) block-size)
;                                        (* (:y b) block-size))) blocks)]
;       (log thing))
;     (<= (+ (* ny block-size) block-size) HEIGHT))
;   (move [block blocks dx dy]
;     (let [{x :x y :y} block
;           nx (+ x dx)
;           ny (+ y dy)]
;       (if (can-move? block blocks nx ny)
;         (->Block nx ny)
;         block))))

(defn gen-world []
  (->World (new-formation)
           []))

(defn draw-block [{x :x y :y} ctx]
  (set! (.-fillStyle ctx) "rgb(0, 0, 0)")
  (.fillRect ctx (* x block-size) (* y block-size) block-size block-size))

(go
  (loop [world (gen-world)]
    (<! (timeout 300))
    (.clearRect context 0 0 WIDTH HEIGHT)
    (let [new-world (fall (:curr-formation world) world)]
      (doseq [block (into (:blocks new-world)
                          (get-in new-world [:curr-formation :blocks]))]
        (draw-block block context))
    (recur new-world))))
