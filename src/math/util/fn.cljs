(ns math.util.fn)

(defn double-equal [n1 n2]
    (< (Math/abs (- n1 n2)) 0.001))

(defn mxv [m v]
  (mapv #(apply + (map * % v)) m))

(defn apply-matrix [point-list matrix vector]
  (mapv #(mapv + (mxv matrix %) vector) point-list))

(defn scale [point-list base-point scl]
  (let [m [[scl 0 0] [0 scl 0] [0 0 scl]]]
    (apply-matrix point-list m (mapv - base-point (mxv m base-point)))))

(defn translate [point-list vector]
  (apply-matrix point-list [[1 0 0] [0 1 0] [0 0 1]] vector))

(defn create-id-generator []
  (let [next-id (atom 0)]
    (fn [] (swap! next-id inc) @next-id)))