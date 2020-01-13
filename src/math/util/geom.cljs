(ns math.util.geom
  (:require [math.util.fn :as func]))

(defn gab->rectangle [[p1 p2]]
    (let [[x1 y1] p1
          [x2 y2] p2]
      [[x1 y1] [x1 y2] [x2 y2] [x2 y1]]))

(defn get-rectangle-lines [points]
  (mapv vec (partition 2 1 points points)))

(defn perp-vec [[i j]] [j i])

(defn distance
  ([[x y]]
    (.sqrt js/Math. (+ (.pow js/Math. x 2) (.pow js/Math. y 2))))
  ([[x1 y1] [x2 y2]]
    (.sqrt js/Math. (+ (.pow js/Math. (- x2 x1) 2) (.pow js/Math. (- y2 y1) 2)))))

(defn normalize [[i j]]
  (let [module (.sqrt js/Math.(+ (.pow js/Math. i 2) (.pow js/Math. j 2)))]
    [(/ i module) (/ j module)]))

(defn angle-between-vectors [v1 v2]
  (Math/acos
    (/ (apply + (map * v1 v2))
       (* (distance v1) (distance v2)))))

(defn is-vectors-co-direct? [v1 v2]
  (func/double-equal
    (angle-between-vectors v1 v2)
    0.0))

(defn get-point-in-param [[p1 p2] param]
  (let [[x y] p1
        [i j] (normalize (mapv - p2 p1))
        k     (* param (distance p1 p2))]
    [(+ x (* i k))
     (+ y (* j k))]))

(defn move-point-by-vector [point dir dist]
  (let [[x y] point
        [i j] (normalize dir)]
    [(+ x (* i dist))
     (+ y (* j dist))]))

(defn lengthen-line [[p1 p2] dist1 dist2]
  (let [direction (mapv - p2 p1)]
    (mapv (fn [point dist]
      (move-point-by-vector point direction dist)) [p1 p2] [dist1 dist2])))

(defn line-inters [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [det (float (- (* (- x2 x1) (- y4 y3))
                      (* (- y2 y1) (- x4 x3))))]
    (if (not= 0.0 det)
      (let [a (/ (- (* (- x4 x3) (- y1 y3))
                    (* (- x1 x3) (- y4 y3)))
                  det)
            b (/ (- (* (- x2 x1) (- y1 y3))
                    (* (- x1 x3) (- y2 y1)))
                  det)]
        (when (and (<= 0.0 a) (<= 0.0 b) (<= a 1.0) (<= b 1.0))
          (get-point-in-param [[x1 y1] [x2 y2]] a))))))

(defn inters-ray-with-line [[[x1 y1] [x2 y2]] [x y] [i j]]
  (let [[dx dy] [(+ x i) (+ y j)]
        det     (float (- (* (- x2 x1) (- dy y))
                          (* (- y2 y1) (- dx x))))]
    (if (not= 0.0 det)
      (let [a (/ (- (* (- dx x) (- y1 y))
                    (* (- x1 x) (- dy y)))
                  det)]
        (when (and (<= 0.0 a) (<= a 1.0))
          (get-point-in-param [[x1 y1] [x2 y2]] a))))))

