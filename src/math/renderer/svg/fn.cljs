(ns math.renderer.svg.fn
    (:require [math.util.dom :as dom]))

(defn add-id [id m]
  (if id
    (conj {:id id} m)
    m))

(defn create-svg-element [element attr-map]
  (let [element (.createElementNS js/document
                 "http://www.w3.org/2000/svg" (name element))]
    (doseq [[k v] attr-map]
      (.setAttribute element (name k) v))
    element))

(defn create-svg-line [{:keys [id class points stroke stroke-width]}]
    (let [p1     (mapv str (nth points 0))
          p2     (mapv str (nth points 1))]
      (create-svg-element
          :line
          (add-id id
            {:class        class
             :x1           (nth p1 0)
             :y1           (nth p1 1)
             :x2           (nth p2 0)
             :y2           (nth p2 1)
             :stroke       (dom/stroke stroke)
             :stroke-width (str stroke-width)}))))

(defn create-svg-text [{:keys [id class text point]}]
  (let [[x y] point]
    (dom/inner-html
      (create-svg-element
        :text
        (add-id id {:class class :x x :y y}))
      text)))

(defn create-svg-path [{:keys [id class d translate fill stroke stroke-width]}]
  (create-svg-element
    :path
    (add-id id
       {:class        class
        :transform    (dom/vec->translate translate)
        :d            d
        :stroke-width stroke-width
        :fill         fill
        :stroke       (dom/stroke stroke)})))

(defn create-svg-circle [{:keys [id class point translate r fill stroke]}]
  (let [[cx cy] point]
    (create-svg-element
      :circle
      {:id        id
       :class     class
       :transform (dom/vec->translate translate)
       :cx        cx
       :cy        cy
       :r         r
       :fill      fill
       :stroke    (dom/stroke stroke)})))