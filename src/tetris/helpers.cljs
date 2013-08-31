(ns tetris.helpers
  (:require [goog.events :as events]
            [goog.events.EventType]
            [cljs.core.async :refer [chan put!]])
  (:import goog.events.EventType))

(def keyword->event-type
  {:keyup goog.events.EventType.KEYUP
   :keydown goog.events.EventType.KEYDOWN
   :keypress goog.events.EventType.KEYPRESS
   :click goog.events.EventType.CLICK
   :dblclick goog.events.EventType.DBLCLICK
   :mousedown goog.events.EventType.MOUSEDOWN
   :mouseup goog.events.EventType.MOUSEUP
   :mouseover goog.events.EventType.MOUSEOVER
   :mouseout goog.events.EventType.MOUSEOUT
   :mousemove goog.events.EventType.MOUSEMOVE
   :focus goog.events.EventType.FOCUS
   :blur goog.events.EventType.BLUR})

(defn listen
  ([el type] (listen el type nil))
  ([el type f] (listen el type f (chan)))
  ([el type f out]
   (events/listen el (keyword->event-type type)
                  (fn [e] (when f (f e)) (put! out e)))
   out))

(defn by-id [id]
  (.getElementById js/document id))

(defn log [& args]
  (.log js/console (clj->js args)))
