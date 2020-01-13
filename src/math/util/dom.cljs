(ns math.util.dom
  (:require [cljs.reader :as reader]))

(defn get-element-by-id [id]
  (.getElementById js/document (name id)))

(defn get-elements-by-class [class]
  (array-seq (.getElementsByClassName js/document class)))

(defn get-checked-radio [radio-name]
  (.querySelector js/document (str "input[name=\"" radio-name "\"]:checked")))

(defn data-set [el data value] (aset (.-dataset el) data value) el)

(defn data-get [el data] (aget (.-dataset el) data))

(defn stroke [stoke-vector]
  (str "rgb(" (clojure.string/join "," stoke-vector) ")"))

(defn inner-html [element data]
  (set! (.-innerHTML element) (str data))
  element)

(defn vec->translate [v]
  (str "translate(" (clojure.string/join " " v)")"))

(defn translate->vec [s]
  (vec (reader/read-string (subs s 9))))

(def svg-point (atom false))

(defn svg-mouse-posi [evt]
  (let [svg  (.-target evt)
        pnt  (if @svg-point
               @svg-point
               (do (reset! svg-point (.createSVGPoint svg)) @svg-point))
        mtx  (.getScreenCTM svg)
        imtx (.inverse mtx)
        x    (.-clientX evt)
        y    (.-clientY evt)]
    (aset pnt "x" x)
    (aset pnt "y" y)
    (reset! svg-point (.matrixTransform pnt imtx)))
    [(.-x @svg-point) (.-y @svg-point)])

(defn map-position-to [pos element]
  [(- (first pos) (.-left (.getBoundingClientRect element)))
   (- (second pos) (.-top (.getBoundingClientRect element)))])

(defn delete-elements-by-class [class]
  (loop [elements (get-elements-by-class class)]
    (if-let [element (first elements)]
      (do (.removeChild (.-parentNode element) element)
          (recur (get-elements-by-class class))))))