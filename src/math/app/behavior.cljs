(ns math.app.behavior)

(defprotocol IMouseDown
  (on-mouse-down [this *app *data e]))

(defprotocol IMouseMove
  (on-mouse-move [this *app *data e]))

(defprotocol IMouseUp
  (on-mouse-up [this *app *data e]))

(defprotocol IMouseWheel
  (on-mouse-wheel [this *app *data e]))
