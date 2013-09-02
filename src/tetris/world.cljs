(ns tetris.world)

(def block-size 30)

(def WIDTH (* 10 block-size))
(def HEIGHT (* 22 block-size))

(defn get-block-at [world [x y]]
  (first (filter (fn [block] (and (= x (:x block))
                                  (= y (:y block))))
                 (:blocks world))))
