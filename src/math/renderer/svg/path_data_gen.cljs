(ns math.renderer.svg.path-data-gen
    (:require [math.util.geom :as geom]))

(defn generate-arc-path [[x1 y1] [x2 y2] [x3 y3]]
  (let [l1      (geom/distance [x3 y3] [x2 y2])
        l2      (geom/distance [x2 y2] [x1 y1])
        l3      (geom/distance [x1 y1] [x3 y3])
        a       (.acos js/Math (/ (- (+ (* l1 l1) (* l2 l2)) (* l3 l3))
                                  (* 2 l1 l2)))
        k       (* 0.5 l1 l2 (.sin js/Math a))
        r       (/ (.round js/Math (* (/ (* l1 l2 l3) 4.0 k) 1000)) 1000)
        laf     (if (> (/ (.-PI js/Math) 2) a) 1 0)
        saf     (if (< (- (* (- x3 x1) (- y2 y1)) (* (- y3 y1) (- x2 x1))) 0) 1 0)]
    (clojure.string/join " " ["M" (str x1 "," y1) "A" r r 0 laf saf (str x3 "," y3)])))

(defn generate-bracket-path [[x1 y1] [x2 y2] [x3 y3] [x4 y4]]
  (clojure.string/join " " ["M" x1 y1 "L" x2 y2 x3 y3 x4 y4]))