(ns math.app.core
  (:require [math.app.behavior :as behavior]
            [math.math-space.core :as space]
            [math.renderer.svg.core :refer (renderer-math-space)]))

(defrecord App [svg *math-space *math-data behaviors]
  behavior/IMouseDown
  (on-mouse-down [app *app *data e]
    (doseq [b (:behaviors app)
            :when (satisfies? behavior/IMouseDown b)]
      (behavior/on-mouse-down b *app *data e)))

  behavior/IMouseMove
  (on-mouse-move [app *app *data e]
    (.preventDefault e)
    (doseq [b (:behaviors app)
            :when (satisfies? behavior/IMouseMove b)]
      (behavior/on-mouse-move b *app *data e)))

  behavior/IMouseUp
  (on-mouse-up [app *app *data e]
    (doseq [b (:behaviors app)
            :when (satisfies? behavior/IMouseUp b)]
      (behavior/on-mouse-up b *app *data e)))

  behavior/IMouseWheel
  (on-mouse-wheel [app *app *data e]
    (doseq [b (:behaviors app)
            :when (satisfies? behavior/IMouseWheel b)]
      (behavior/on-mouse-wheel b *app *data e))))

(defn init [canvas]
  (let [*math-space (atom (space/empty-math-space canvas {:bind :centr :offset [0 0]}))
        *math-data  (atom {})
        *app        (atom (App. canvas *math-space *math-data #{}))]
    (doto canvas
      (.addEventListener "mousedown" #(behavior/on-mouse-down @*app *math-space *math-data %))
      (.addEventListener "mousemove" #(behavior/on-mouse-move @*app *math-space *math-data %))
      (.addEventListener "mouseup" #(behavior/on-mouse-up @*app *math-space *math-data %))
      (.addEventListener "mousewheel" #(behavior/on-mouse-wheel @*app *math-space *math-data %)))
    (add-watch *math-space :renderer
      (fn [key atom old new]
        (when (or (not= (:scale old) (:scale new))
                  (not= (:start-point old) (:start-point new))
                  (not= (-> old :objects keys) (-> new :objects keys)) )
          (renderer-math-space canvas new))))
    *app))

(defn add-behavior [*app behavior]
  (swap! *app update :behaviors conj behavior))
