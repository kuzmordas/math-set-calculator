(ns math.renderer.svg.core
    (:require [math.renderer.svg.axis-data-gen :as axis-gen]
              [math.renderer.svg.path-data-gen :as path-gen]
              [math.renderer.svg.fn            :as svg-fn]
              [math.util.geom                  :as geom]
              [math.util.dom                   :as dom]))

(defn clear [canvas]
  (while (.-firstChild canvas) (.removeChild canvas (.-firstChild canvas))))

(defn is-point-in-view? [[x y] [[x1 x2] [y1 y2]]]
  (every? true? [(>= x x1) (<= x x2) (>= y y1) (<= y y2)]))

(defmulti renderer
  (fn [id object math-space] (:type object)))

(defmethod renderer :axis [id object {:keys [start-point size scale]}]
  (let [axis-points (axis-gen/find-axis-points
                      (-> size geom/gab->rectangle geom/get-rectangle-lines) start-point (:angle object))]
    (when (= (count axis-points) 2)
      (into
        [(svg-fn/create-svg-line
          {:id (name id) :class "axis" :points axis-points :stroke-width 3 :stroke [100 100 100]})]
        (apply concat
          (mapv
            (fn [[num point line]]
              [(svg-fn/create-svg-text {:class "axis text" :text (str num) :point (mapv + [3 -7] point)})
               (svg-fn/create-svg-line {:class "axis nick" :points line :stroke-width 1 :stroke [100 100 100]})])
            (axis-gen/make-axis-nick-data start-point scale axis-points)))))))

;; TODO change dataset implementation for math-elements
(defmethod renderer :bracket [id object {:keys [start-point scale]}]
  (let [point  (mapv * (:point object) scale)
        points (if (:left? object)
                 [(mapv + start-point [5 20])  start-point  (mapv + start-point [5 -20])]
                 [(mapv + start-point [-5 20]) start-point (mapv + start-point [-5 -20])])
        d      (apply path-gen/generate-arc-path points)]
    (dom/data-set 
      (svg-fn/create-svg-path
        {:id (name id) :class "bracket" :d d :translate point :fill "transparent" :stroke [0 0 0] :stroke-width 3}) "set" (:set object))))

(defmethod renderer :square-bracket [id object {:keys [start-point scale]}]
  (let [[x y]  start-point
        point  (mapv * (:point object) scale)
        points (if (:left? object)
                 [[(+ 7 x) (- y 20)] [x (- y 20)] [x (+ y 20)] [(+ 7 x) (+ y 20)]]
                 [[(- x 7) (- y 20)] [x (- y 20)] [x (+ y 20)] [(- x 7) (+ y 20)]])
        d      (apply path-gen/generate-bracket-path points)]
    (dom/data-set 
      (svg-fn/create-svg-path
        {:id (name id) :class "square-bracket" :d d :translate point :fill "transparent" :stroke [0 0 0] :stroke-width 3}) "set" (:set object))))

(defmethod renderer :point [id object {:keys [start-point scale]}]
  (let [point  (mapv * (:point object) scale)]
    (dom/data-set 
      (svg-fn/create-svg-circle
        {:id (name id) :class "point" :point start-point :translate point :r 5 :fill "black" :stroke [0 0 0]}) "set" (:set object))))

(defn renderer-math-space [canvas math-space]
  (clear canvas)
    (doseq [[id obj] (:objects math-space)]
      (let [svg-elem (renderer id obj math-space)] ;; todo add check object in view
        (if (vector? svg-elem)
          (doseq [e svg-elem] (.appendChild canvas e))
          (do
            ;; (.dataset svg-elem )
            (.appendChild canvas svg-elem))
          ))))