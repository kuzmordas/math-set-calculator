(ns math.logic.set-calc.core)

(defn double-equal [n1 n2]
    (< (Math/abs (- n1 n2)) 0.0001))

(defn sort-intervals [intervals]
    (vec (sort
      (fn [x y]
        (if (double-equal (-> x first second) (-> y first second))
          (< (-> x first first) (-> y first first))
          (> (-> x first second) (-> y first second)))) intervals)))

(defn border-contact?
  [e1 e2]
  (let [[[a1 b1] [a2 b2]] [e1 e2]]
    (or
      (and (double-equal (second b1) (second a2))
           (or (and (= (first b1) 1) (= (first a2) -1))
               (and (= (first b1) -1) (= (first a2) 1))))
      (and (double-equal (second a1) (second b2))
           (or (and (= (first a1) 1) (= (first b2) -1))
               (and (= (first a1) -1) (= (first b2) 1)))))))

(defn set-in-set? [[a1 b1] [a2 b2]]
  (and
    (<= (second a1) (second a2))
    (if (double-equal (second a1) (second a2))
      (>= 1 (first a1) (first a2))
      true)
    (>= (second b1) (second b2))
    (if (double-equal (second b1) (second b2))
      (>= 1 (first b1) (first b2))
      true)))

(defn common-border?
  [e1 e2]
  (let [[[a1 b1] [a2 b2]] (sort-intervals [e1 e2])]
    (or (and (double-equal (second a1) (second b2)) (= (first a1) (first b2) 1))
        (and (double-equal (second b1) (second a2)) (= (first b1) (first a2) 1)))))

(defn overlap?
  [e1 e2]
  (let [[[a1 b1] [a2 b2]]
          (sort-intervals [e1 e2])]
   (or (common-border? e1 e2)
      (and (> (second b1) (second a2))
        (> (second b2) (second a1))))))

(defn set-union2 [e1 e2]
  (cond
    (empty? e1) [e2]
    (empty? e2) [e1]
  :else
  (if (or (overlap? e1 e2)
          (common-border? e1 e2)
          (border-contact? e1 e2))
    (let [[left1 right1] e1
          [left2 right2] e2
          [[l-type1 l-val1] [r-type1 r-val1]] e1
          [[l-type2 l-val2] [r-type2 r-val2]] e2]
      [[(if (double-equal l-val1 l-val2)
         [(max l-type1 l-type2) l-val1]
         (first (sort #(< (second %1) (second %2)) [left1 left2])))
       (if (double-equal r-val1 r-val2)
         [(max r-type1 r-type2) r-val1]
         (first (sort #(> (second %1) (second %2)) [right1 right2])))]])
    [e1 e2])))

(defn set-intersect2 [e1 e2]
  (if (or (empty? e1) (empty? e2))
  nil
  (if (or (overlap? e1 e2)
          (set-in-set? e1 e2)
          (set-in-set? e2 e1)
          (common-border? e1 e2))
    (let [[left1 right1] e1
        [left2 right2] e2
        [[l-type1 l-val1] [r-type1 r-val1]] e1
        [[l-type2 l-val2] [r-type2 r-val2]] e2]
      [(if (double-equal l-val1 l-val2)
         [(min l-type1 l-type2) l-val1]
          (first (sort #(> (second %1) (second %2)) [left1 left2])))
       (if (double-equal r-val1 r-val2)
         [(min r-type1 r-type2) r-val1]
          (first (sort #(< (second %1) (second %2)) [right1 right2])))])
    nil)))

(defn set-substract2 [e1 e2]
  (cond
    (set-in-set? e2 e1) nil
    (not (overlap? e1 e2)) [e1]
    :else
      (let [[left1 right1] e1
            [left2 right2] e2
            [[l-type1 l-val1] [r-type1 r-val1]] e1
            [[l-type2 l-val2] [r-type2 r-val2]] e2]
      (cond
        (and (< l-val1 l-val2) (< r-val1 r-val2)) [[[l-type1 l-val1] [(* -1 l-type2) l-val2]]]
        (and (> r-val1 r-val2) (> l-val1 l-val2)) [[[(* -1 r-type2) r-val2] [r-type1 r-val1]]]
        :else
          (cond
            (double-equal l-val1 l-val2)
              (if (or (<= l-type1 l-type2))
                [[[(* -1 r-type2) r-val2] [r-type1 r-val1]]]
                [[[1 l-val1] [1 l-val1]] [[(* -1 r-type2) r-val2] [r-type1 r-val1]]])
            (double-equal r-val1 r-val2)
              (if (<= r-type1 r-type2)
                [[[l-type1 l-val1] [(* -1 l-type2) l-val2]]]
                [[[l-type1 l-val1] [(* -1 l-type2) l-val2]] [[1 r-val1] [1 r-val1]]])
            :else
              [[[l-type1 l-val1] [(* -1 l-type2) l-val2]] [[(* -1 r-type2) r-val2] [r-type1 r-val1]]])))))

(defmulti operation
  (fn [operation-type set sets] operation-type))

(defmethod operation :union
  [operation-type set sets]
  (let [sets (sort-intervals (conj sets set))]
    (reduce
      (fn [res s]
        (let [r (set-union2 (last res) s)]
          (if (second r)
            (conj res (second r))
            (conj (vec (butlast res)) (first r)))))
      [(first sets)]
      (next sets))))

(defmethod operation :substract
  [operation-type set sets]
  (reduce
    (fn [res s1]
      (if-let [s2 (set-substract2 s1 set)]
        (vec (concat res s2))
        res))
    []
    (sort-intervals sets)))

(defmethod operation :intersect
  [operation-type set sets]
  (reduce
    (fn [res s1]
      (if-let [s2 (set-intersect2 s1 set)]
        (conj res s2)
        res))
    []
    (sort-intervals sets)))

(defn operation-test [operations sets]
  (loop [opers operations
         result [(:a sets)]]
    (if-let [[o s] (first opers)]
      (do
        (println s (operation o (s sets) result))
        (recur (vec (next opers)) (operation o (s sets) result)))
    result)))
