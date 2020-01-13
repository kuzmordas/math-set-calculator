(ns math.math-space.core)

(defrecord MathSpace [canvas start-point scale size objects])

(def bind-functions
  {:centr (fn [size offset] (mapv + (mapv #(/ % 2.0) size) offset))
   :left  (fn [[x y] offset] (mapv + [0 (/ y 2.0)] offset))
   :right (fn [[x y] [a b]] [(- x a) (+ y b)])})

(defn empty-math-space [canvas {:keys [bind offset]}]
  (let [width  (js/parseFloat (.getAttribute canvas "width"))
        height (js/parseFloat (.getAttribute canvas "height"))]
    (MathSpace.
      canvas
      ((bind bind-functions) [width height] offset)
      [100 100]
      [[0 0] [width height]]
      {})))
