(ns math.ui.app
  (:require
    [math.util.dom            :as dom]
    [math.util.fn             :as func]
    [math.ui.util             :as util]
    [math.logic.set-calc.core :as calc]))

(def add-element-button (dom/get-element-by-id "add-element-to-set"))
(def left-math-sym (dom/get-element-by-id "left-interval-symbols"))
(def right-math-sym (dom/get-element-by-id "right-interval-symbols"))
(def eval-button (dom/get-element-by-id "eval"))
(def set-list (dom/get-element-by-id "set-elements"))
(def operations-stack (dom/get-element-by-id "boolean-operations-stack"))
(def generate-math-element-id! (func/create-id-generator))
(def generate-element-id! (func/create-id-generator))

(.addEventListener
  left-math-sym
  "mousewheel"
  (fn [e]
    (.preventDefault e)
    (set! (.-checked (dom/get-element-by-id (str "left-interval-border-" (if (> (.-wheelDelta e) 0) 1 2)))) true)))

(.addEventListener
  right-math-sym
  "mousewheel"
  (fn [e]
    (.preventDefault e)
    (set! (.-checked (dom/get-element-by-id (str "right-interval-border-" (if (> (.-wheelDelta e) 0) 1 2)))) true)))

(defn add-math-interval [*math-space *math-data set-list [l-border r-border] [l-value r-value]]
  (let [l-border-type     (if (= l-border "(") :bracket :square-bracket)
        r-border-type     (if (= r-border ")") :bracket :square-bracket)
        set-element       (char (+ 96 (generate-math-element-id!)))
        operation         (util/get-boolean-operation)
        [geom-e1 geom-e2] [(str "element-" (generate-element-id!)) (str "element-" (generate-element-id!))]]
    (util/add-element-to-list
      (str "set-element-" set-element) set-list (str l-border l-value ", " r-value r-border))
    (swap! *math-data
      assoc-in [:set-elements (keyword set-element)] [geom-e1 geom-e2])
    (if (not= 1 (count (keys (:set-elements @*math-data))))
      (do (util/add-element-to-stack nil operations-stack (peek operation))
          (swap! *math-data update-in [:operations] #(vec (conj % (conj (pop operation) (keyword set-element)))))))
    (util/add-element-to-stack
      (str "stack-element-" set-element) operations-stack (clojure.string/upper-case set-element))
    (swap! *math-space
      assoc-in [:objects (keyword geom-e1)]
      {:type l-border-type :point [l-value 0] :set (str set-element) :left? true})
    (swap! *math-space
      assoc-in [:objects (keyword geom-e2)]
      {:type r-border-type :point [r-value 0] :set (str set-element)})))

(defn add-math-discret [*math-space *math-data set-list value]
  (let [set-element  (char (+ 96 (generate-math-element-id!)))
        geom-element (str "element-" (generate-element-id!))
        operation    (util/get-boolean-operation)]
    (util/add-element-to-list
      (str "set-element-" set-element) set-list (str "{" value "}"))
    (swap! *math-data
      assoc-in [:set-elements (keyword set-element)] [geom-element])
    (if (not= 1 (count (keys (:set-elements @*math-data))))
      (do (util/add-element-to-stack nil operations-stack (peek operation))
          (swap! *math-data
            update-in [:operations] #(vec (conj % (conj (pop operation) (keyword set-element)))))))
    (util/add-element-to-stack
      (str "stack-element-" set-element) operations-stack (clojure.string/upper-case set-element))
    (swap! *math-space
      assoc-in [:objects (keyword geom-element)]
      {:type :point :point [value 0] :set (str set-element)})))

(defn add-math-element [*app]
  (let [*math-space (:*math-space @*app)
        *math-data  (:*math-data @*app)]
    (.addEventListener add-element-button "click"
      (fn []
        (let [elements (util/get-elements-for-set)]
          (when-let [value (:discret elements)]
            (add-math-discret *math-space *math-data set-list value))
          (when (every? #(not= % nil) (:interval elements))
            (add-math-interval *math-space *math-data set-list (util/get-interval-borders) (:interval elements))))))))

(defn get-set-info [math-space math-data]
  {:math-set (reduce
                (fn [result k]
                  (assoc result k
                    (let [elements (k (:set-elements math-data))]
                      (if (= (count elements) 2)
                        (mapv (fn [y]
                          (let [object ((keyword y) (:objects math-space))]
                            [(if (= (:type object) :square-bracket) 1 -1) (first (:point object))])) elements)
                        (let [object ((keyword (first elements)) (:objects math-space))]
                          [[1 (first (:point object))] [1 (first (:point object))]])))))
                {}
                (keys (:set-elements math-data)))
    :operations (:operations math-data)})

(defn draw-set [data *math-space math-data]
  (doseq [e (apply concat (vals (:set-elements math-data)))]
    (swap! *math-space
      update-in [:objects] dissoc (keyword e)))
  (doseq [[[type1 p1] [type2 p2]] data]
    (if (calc/double-equal p1 p2)
      (swap! *math-space
        assoc-in [:objects (keyword (str "element-" (generate-element-id!)))]
        {:type :point :point [p1 0] :set "result"})
      (do
        (swap! *math-space
          assoc-in [:objects (keyword (str "element-" (generate-element-id!)))]
          {:type (if (= type1 -1) :bracket :square-bracket) :point [p1 0] :set "result" :left? true})
        (swap! *math-space
          assoc-in [:objects (keyword (str "element-" (generate-element-id!)))]
          {:type (if (= type2 -1) :bracket :square-bracket) :point [p2 0] :set "result"}) ))))

(defn set-calc [*app]
  (.addEventListener eval-button "click"
    (fn []
      (let [*math-space (:*math-space @*app)
            *math-data  (:*math-data @*app)
            data        (get-set-info @*math-space @*math-data)
            result      (calc/operation-test (:operations data) (:math-set data))]
        (println data)
        (println result)
        (draw-set result *math-space @*math-data)))))