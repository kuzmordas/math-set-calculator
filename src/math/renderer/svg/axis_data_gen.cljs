(ns math.renderer.svg.axis-data-gen
    (:require [math.util.geom :as geom]))

(defonce nick-size 7)

(defn find-axis-points [canvas-borders start-point angle]
  (reduce
    (fn [points border]
      (if-let [point (geom/inters-ray-with-line border start-point angle)]
        (conj points point)
        points))
    []
    canvas-borders))

(defn make-axis-nick-data [start-point [sx sy] [p1 p2]]
  (let [axis-vector (mapv - p2 p1)
        axis-dist   (geom/distance p1 p2)
        axis-perp   (geom/perp-vec (geom/normalize axis-vector))
        co-direct1? (geom/is-vectors-co-direct?
                      axis-vector (mapv - p1 start-point))
        co-direct2? (geom/is-vectors-co-direct?
                      axis-vector (mapv - p2 start-point))
        num1        ((if co-direct1? + -) (quot (geom/distance start-point p1) sx))
        num2        ((if co-direct2? + -) (quot (geom/distance start-point p2) sx))
        first-point (geom/move-point-by-vector start-point axis-vector (* num1 sx))]
   (loop [result [[(int num1)
                   first-point
                   [(geom/move-point-by-vector first-point axis-perp nick-size)
                    (geom/move-point-by-vector first-point axis-perp (- nick-size))]]]]
     (let [point (geom/move-point-by-vector (second (last result)) axis-vector sx)
           num   (first (last result))]
       (if (<= (geom/distance point first-point) axis-dist)
        (recur (conj result
                 [(inc num) point [(geom/move-point-by-vector point axis-perp nick-size) (geom/move-point-by-vector point axis-perp (- nick-size))]]))
        (conj result
          [(inc num) point [(geom/move-point-by-vector point axis-perp nick-size) (geom/move-point-by-vector point axis-perp (- nick-size))]]))))))