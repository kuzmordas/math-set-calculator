(ns math.ui.util
  (:require
    [math.util.dom :as dom]
    [math.util.fn  :as func]))

(defn get-value-from-input [input-id]
  (let [val (.-value (dom/get-element-by-id input-id))]
    (if-not (= val "")
      (js/parseFloat val))))

(defn add-element-to-list [elem-id list-elem text]
  (let [li (.createElement js/document "li")]
    (set! (.-innerHTML li) text)
    (set! (.-id li) elem-id)
    (.add (.-classList li) "math-element")
    (.appendChild list-elem li)))

(defn add-element-to-stack [elem-id stack-elem text]
  (let [span (.createElement js/document "span")]
    (set! (.-innerHTML span) text)
    (if elem-id (set! (.-id span) elem-id))
    (.add (.-classList span) "math-set-ui__stack-item")
    (.appendChild stack-elem span)))

(defn get-count-of-list [list-elem]
  (-> (.getElementsByTagName list-elem "li") array-seq count))

(defn get-elements-for-set []
  (let [e1 [(get-value-from-input "start-value")
            (get-value-from-input "end-value")]
        e2  (get-value-from-input "discret-value")]
     {:interval e1 :discret e2}))

(defn get-interval-borders []
  [(.-value (dom/get-checked-radio "left-interval-border"))
   (.-value (dom/get-checked-radio "right-interval-border"))])

(defn get-boolean-operation []
  (let [input-elem (dom/get-checked-radio "boolean-operations")]
    [(keyword (.-id input-elem)) (.-value input-elem)]))