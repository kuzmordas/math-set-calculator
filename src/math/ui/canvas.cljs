(ns math.ui.canvas
  (:require
    [math.app.behavior :as behavior]
    [math.app.core     :as app]
    [math.util.dom     :as dom]
    [math.util.fn      :as func]))

(defn valid-target? [elem]
  (some #{(first (clojure.string/split (.getAttribute elem "class") #" "))} ["point" "bracket" "square-bracket"]))

(defn set-math-element-position [elem start-point scale]
  (mapv #(.toFixed (/ (- %1 %2) %3) 2)
    (mapv + start-point (dom/translate->vec (.getAttribute elem "transform"))) start-point scale))

(defn change-element-position-in-list [elem math-space math-data]
  (let [set-elem  (dom/data-get elem "set")
        li-elem   (dom/get-element-by-id (str "set-element-" set-elem))
        coords    (mapv #(:point ((keyword %) (:objects math-space)))
                    ((keyword set-elem) (:set-elements math-data)))]
  (set! (.-innerHTML li-elem)
    (if (= (count coords) 2)
      (str "[" (clojure.string/join ", " (mapv #(-> % first) coords)) "]")
      (str "{" (-> coords first first str) "}")))))

(defn change-element-color-in-stack [elem mode]
  (let [stack-elem-id (str "stack-element-" (dom/data-get elem "set"))
        stack-elem    (dom/get-element-by-id stack-elem-id)]
    (if mode
      (.add (.-classList stack-elem) "math-set-ui__stack-item_active")
      (.remove (.-classList stack-elem) "math-set-ui__stack-item_active"))))

(def canvas-behavior
  (let [*drag (atom false)
        *dv   (atom nil)]
    (reify
      behavior/IMouseDown
      (on-mouse-down [_ *math-space *math-data e]
        (when (valid-target? (.-target e))
          (reset! *drag (.-target e))
          (reset! *dv
            (mapv - (dom/map-position-to [(.-clientX e) (.-clientY e)] (:canvas @*math-space))
                    (mapv + (:start-point @*math-space) (dom/translate->vec (.getAttribute @*drag "transform")))))))

      behavior/IMouseMove
      (on-mouse-move [_ *math-space *math-data e]
        (when @*drag
          (let [[sx xy] (:start-point @*math-space)
                [x y]   (dom/map-position-to [(.-clientX e) (.-clientY e)] (:canvas @*math-space))]
            (.setAttribute @*drag "transform" (dom/vec->translate [(- x (first @*dv) sx) 0]))
            (swap! *math-space assoc-in
              [:objects (keyword (.-id @*drag)) :point]
              (set-math-element-position @*drag (:start-point @*math-space) (:scale @*math-space)))
            (change-element-color-in-stack @*drag true)
            (change-element-position-in-list @*drag @*math-space @*math-data))))

      behavior/IMouseUp
      (on-mouse-up [_ *math-space *math-data e]
        (change-element-color-in-stack @*drag false)
        (reset! *drag false)
        (reset! *dv nil))

      behavior/IMouseWheel
      (on-mouse-wheel [_ *math-space *math-data e]
        (let [[x y]    (dom/map-position-to [(.-clientX e) (.-clientY e)] (:canvas @*math-space))
              [sx sy] (:scale @*math-space)
              k       (if (neg? (.-wheelDelta e)) 0.95 1.05)]
          (if (and (or (= k 0.95) (< sx 200.0)) (or (= k 1.05) (> sx 30.0)))
            (do (swap! *math-space
                  update-in [:start-point] #(first (func/scale [%] [x 100] k))) ;;todo: pass y
                (swap! *math-space
                  update-in [:scale] #(mapv (fn [x] (* x k)) %)))))))))
