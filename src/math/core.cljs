(ns math.core
  (:require
    [math.app.core  :as app]
    [math.ui.app    :as ui]
    [math.ui.canvas :as canvas]
    [math.util.dom  :as dom]))

(defn start-set-calc []
  (let [svg (dom/get-element-by-id "imath")
        *app (app/init svg)]
    (swap! (:*math-space @*app)
      assoc-in [:objects :x-axis] {:type :axis :angle [1 0]})
    (app/add-behavior *app canvas/canvas-behavior)
    (ui/add-math-element *app)
    (ui/set-calc *app)))

(start-set-calc)

(js/console.log "set calculator loaded")
