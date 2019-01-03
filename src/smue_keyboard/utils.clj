(ns smue-keyboard.utils
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))



(def switch-cuttout-size 14.3)
(def switch-plane-size 21)
(def case-thickness 4)
(def r  (* 0.5 case-thickness))


(defn acrd
  "the inverse of the chord function
   given segmentlength of two points on circle with radius=1
  returning the angle"
  [y]
  (* 2.0 (Math/asin (/ y 2.0))))


(defn row-phi
  "given length between two points on circle with given radius
  returns the angle beween those points on that circle"
  [panel-size radius]
  (acrd (/ panel-size radius)))


(defn R
  "rotation matrix"
  [dim phi]
  (case dim
    :x [[1 0 0]
        [0 (Math/cos phi) (- (Math/sin phi))]
        [0 (Math/sin phi) (Math/cos phi)]]
    :y [[(Math/cos phi) 0 (Math/sin phi)]
        [0 1 0]
        [(- (Math/sin phi)) 0 (Math/cos phi)]]
    :z [[(Math/cos phi) (- (Math/sin phi)) 0]
        [(Math/sin phi) (Math/cos phi) 0]
        [0 0 1]]))

(defn translate-v
  "adds two vectors elementwise together"
  [v voff]
  (mapv + v voff))



(defn dot
  "dot product of two vectors"
  [v1 v2]
  (reduce + (mapv * v1 v2)))



(defn vmdot
  "vector times matrix vM"
  [v m]
  (vec (for [i (range (count v))]
         (reduce + (map * v (map #(nth % i) m))))))



(defn mvdot
  "matrix times vector Mv"
  [m v]
  [(reduce + (map * (first m) v))
   (reduce + (map * (second m) v))
   (reduce + (map * (nth m 2) v))])



(defn rotate-v
  "totate given vector around given axis and angle
  returns rotated vector"
  [v dim phi]
  (mvdot (R dim phi) v))


(defn cross-p
  "cross product of two vectors"
  [v1 v2]
  (let [[a1 a2 a3] v1
        [b1 b2 b3] v2]
    [(- (* a2 b3) (* a3 b2))
     (- (* a3 b1) (* a1 b3))
     (- (* a1 b2) (* a2 b1))]))



(defn update-values
  "this updates each entry of a map with a given function"
  [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))



(defn mean-point [plate]
  (mapv (partial * 0.25) (mapv + (:bl plate) (:tl plate) (:br plate) (:tr plate))))

(defn mean [vectors]
  (mapv #(/ % (count vectors))
        (reduce #(mapv + %1 %2) vectors)))

(defn length-v [v]
  (Math/sqrt (reduce + (map #(* % %) v))))


(defn normalize-v [v]
  (mapv (partial * (/ 1. (length-v v))) v))

(assert (= [1.0 0.0 0.0] (normalize-v [2 0 0])))

(defn scale-v [v s]
  (mapv (partial * s) v))

(scale-v [1 2 3] 2)

(defn dihedral-angle [b1 b2 dim]
  (let [b3 (dim
            {:x [0 0 1]
             :y [0 0 1]
             :z [0 1 0]})]
    (Math/atan2
     (dot (cross-p (cross-p b1 b2) (cross-p b2 b3))
          (normalize-v b2))
     (dot (cross-p b1 b2)
          (cross-p b2 b3)))))

;; (dihedral-angle [0 1 0] [1 0 0] :z)

(defn sp
  "returns translated sphere"
  [tr]
  (->> (sphere r) (translate tr)))

(defn wall [expr]
  (hull expr
        (->>
         (project expr)
         (extrude-linear {:height 0.0001}))))

(def cap
  (let [a 18
        h 11
        off-y 2.5
        r 2
        off-x 3 
        a1 (- a r r)
        key-cap (hull
                 (->> (minkowski (square a1 a1) (with-fn 30 (circle r)))
                      (extrude-linear {:height 0.5}))
                      (->> (with-fn 30 (sphere r))
                           (translate [(- (* a1 0.5) off-x)
                                       (* a1 0.5)
                                       (dec h)]))
                      (->> (with-fn 30 (sphere r))
                           (translate [(+ off-x (* a1 -0.5))
                                       (* a1 0.5)
                                       (dec h)]))
                      (->> (with-fn 30 (sphere r))
                           (translate [(- (* a1 0.5) off-x)
                                       (+ (* a1 -0.5) off-y)
                                       (dec h)]))
                      (->> (with-fn 30 (sphere r))
                           (translate [(+ off-x (* a1 -0.5))
                                       (+ (* a1 -0.5) off-y)
                                       (dec h)])))]
    (->> (difference
          key-cap
          (->> (with-fn 150 (cylinder [30 30] 22))
               (translate [0 41 0])
               (rotate (* 0.5 Math/PI) [1 0 0])
               ))
         (rotate Math/PI [0 0 1])
         (translate [0 0 (+ 5 case-thickness)])
         (rotate Math/PI [1 0 0])
         (color [220/255 215/255 220/255 1]))))


(defn keyplate-stamp [vtxs]
  (let [baseplates (for [col vtxs
                         p col]
                     (hull
                      (->> (union (sp (:br p))
                                  (sp (:tl p))
                                  (sp (:bl p))
                                  (sp (:tr p)))
                           (translate [0 0 200] ))
                      (sp (:br p))
                      (sp (:tl p))
                      (sp (:bl p))
                      (sp (:tr p))))
        intersections (loop [cols vtxs
                             acc []]
                        (if (= 1 (count cols))
                          acc
                          (let [a (first cols)
                                b (second cols)
                                amax (count a)
                                bmax (count b)
                                n-iter (max amax bmax)]
                            (recur
                             (rest cols)
                             (cons
                              (for [i (range n-iter)]
                                (let [pa (nth a (min (dec amax) i))
                                      pb (nth b (min (dec bmax) i))]
                                  (hull
                                   (->> (union
                                         (sp (:bl pb))
                                         (sp (:tl pb))
                                         (sp (:br pa))
                                         (sp (:tr pa)))
                                        (translate [0 0 200]))
                                   (sp (:bl pb))
                                   (sp (:tl pb))
                                   (sp (:br pa))
                                   (sp (:tr pa)))))
                              acc)))))]
    (union
     baseplates intersections)))