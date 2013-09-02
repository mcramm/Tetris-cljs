(ns tetris.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! chan put! close! timeout alts!]]
            [tetris.helpers :as h]
            [tetris.formations :as formations]
            [tetris.world :refer [block-size WIDTH HEIGHT N-WIDTH N-HEIGHT get-block-at]]))

(def canvas (h/by-id "canvas"))
(set! (.-width canvas) WIDTH)
(set! (.-height canvas) HEIGHT)
(def next-block (h/by-id "next-block"))
(set! (.-width next-block) N-WIDTH)
(set! (.-height next-block) N-HEIGHT)

(def context (.getContext canvas "2d"))
(def next-block-ctx (.getContext next-block "2d"))


(defrecord World [curr-formation blocks completed-rows next-formation])

(defn gen-world []
  (->World (formations/random-formation)
           []
           0
           (formations/random-formation 1 1)))

(defn draw-block [{x :x y :y color :color} ctx]
  (when-not (nil? x)
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx (* x block-size) (* y block-size) block-size block-size)))

(def events (chan))

(go (while true
      (<! (timeout 700))
      (>! events :drop)))

(go (loop [s 1]
      (<! (timeout 1000))
      (set! (.-innerHTML (h/by-id "seconds")) (format "%02d" (mod s 60)))
      (set! (.-innerHTML (h/by-id "minutes")) (format "%02d" (mod (Math/floor (/ s 60)) 60)))
      (set! (.-innerHTML (h/by-id "hours")) (format "%02d" (Math/floor (/ s 3600))))
      (recur (inc s))))

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

(defn delete-row [world row]
  (loop [blocks (:blocks world)
         new-blocks []]
    (if (empty? blocks)
      new-blocks
      (let [b (first blocks)
            b (cond
                (nil? b) b
                (> (:y b) row) b
                (= (:y b) row) nil
                (< (:y b) row) (formations/create-block (:x b) (inc (:y b)) (:color b)))]
        (recur (rest blocks) (conj new-blocks b))))))

(defn clear-blocks [world]
  (let [first-col-blocks (filter (complement nil?)
                                 (mapv (fn [y] (get-block-at world [0 y]))
                                      (range 0 (/ HEIGHT block-size))))
        poss-row-nums (mapv :y first-col-blocks)
        poss-rows (mapv (fn [y] (mapv (fn [x] (get-block-at world [x y])) (range 0 (/ WIDTH block-size)))) poss-row-nums)
        full-rows (filter #(every? true? (mapv (complement nil?) %))
                          poss-rows)
        full-row-nums (mapv (fn [row] (:y (first row))) full-rows)]

    (if (<= (count full-rows) 0)
      world
      (loop [new-world (assoc world :completed-rows (+ (count full-rows)
                                                       (:completed-rows world)))
             rows full-row-nums]
        (if (empty? rows)
          new-world
          (let [row (first rows)]
            (recur (assoc new-world :blocks (delete-row new-world row)) (rest rows))))))))

(go
  (loop [world (gen-world)]
    (let [event (<! events)]
      (.clearRect context 0 0 WIDTH HEIGHT)
      (.clearRect next-block-ctx 0 0 N-WIDTH N-HEIGHT)
      (let [new-world (condp = event
                        :left (formations/move (:curr-formation world) world -1 0)
                        :right (formations/move (:curr-formation world) world 1 0)
                        :drop (formations/move (:curr-formation world) world 0 1)
                        :rotate (formations/rotate (:curr-formation world) world)
                        world)
            new-world (clear-blocks new-world)]
        (doseq [block (into (:blocks new-world)
                            (formations/translated-blocks (:curr-formation new-world)))]
          (draw-block block context))
        (doseq [block (formations/translated-blocks (:next-formation new-world))]
          (draw-block block next-block-ctx))
        (set! (.-innerHTML (h/by-id "completed-rows")) (:completed-rows new-world))
        (recur new-world)))))
