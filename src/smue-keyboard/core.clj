(ns smue-keyboard.core
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))


(def single-button-angle (* Math/PI (/ 13.0 180)))
(def single-row-angle (* Math/PI (/ 9.0 180)))
(def switch-cuttout-size 14.7)
(def switch-plane-size 26)
(def case-thickness 4)


(defn acrd
  "the inverse of the chord function
   given segmentlength of two points on circle with radius=1
  returning the angle"
  [y]
  (* 2.0 (Math/asin (/ y 2.0))))


(defn row-phi
  [panel-size radius]
  (acrd (/ panel-size radius)))


(defn col-case-boxes [r n-rows a b c]
  (union (for [e (range n-rows)]
           (->> (cube a  c  b)
                (translate [0 r 0])
                (rotate (* e single-button-angle) [1 0 0])))))

(defn distribute [r row shape]
  (->> shape
       (translate [0 r 0])
       (rotate (* row (row-phi switch-plane-size r)) [1 0 0])))



(defn switches-col [r n-rows]
  (let [cutout (cube switch-cuttout-size (+ 1 case-thickness) switch-cuttout-size)]
    (union
     (for [row (range n-rows)]
       (distribute r row cutout)
       ))))

(def cap
  (let [a 18
        h 11
        off-y 2.5
        off-x 1
        key-cap (hull (->> (hull
                            (for [i [-0.5 0.5]
                                  j [-0.5 0.5]]
                              (->> (with-fn 60 (sphere 2))
                                   (translate [(* a i) (* a j) 4]))))
                           (project)
                           (extrude-linear {:height 1.5})
                           (translate [0 0 0.05]))
                      
                      (->> (with-fn 30 (sphere 2))
                           (translate [(- (* a 0.5) off-x)
                                       (* a 0.5)
                                       (dec h)]))
                      (->> (with-fn 30 (sphere 2))
                           (translate [(+ off-x (* a -0.5))
                                       (* a 0.5)
                                       (dec h)]))
                      (->> (with-fn 30 (sphere 2))
                           (translate [(- (* a 0.5) off-x)
                                       (+ (* a -0.5) off-y)
                                       (dec h)]))
                      (->> (with-fn 30 (sphere 2))
                           (translate [(+ off-x (* a -0.5))
                                       (+ (* a -0.5) off-y)
                                       (dec h)])))]
    (->> (difference
          key-cap
          (->> (with-fn 150 (cylinder [30 30] 22))
               (translate [0 41 0])
               (rotate (* 0.5 Math/PI) [1 0 0])
               ))
         (rotate Math/PI [0 0 1])
         (translate [0 0 (+ 5 case-thickness)])
         (rotate (/ 3.14 2) [1 0 0])

         (color [220/255 215/255 220/255 1]))))

(defn caps-col [radius n-rows]
  (union
   (for [row (range n-rows)]
     (distribute radius row cap))))


(defn reposition [col radius n-rows expr]
  (let [t 83
        phi (row-phi switch-plane-size radius)]
    (->> expr
         (rotate -0.34 [1 0 0])
         (translate [0 t 0])
         (rotate (* col single-row-angle) [0 0 1])
         ;; (rotate (* col phi) [0 0 1])
         (translate [0 (- t) 0])
         (rotate 1.4 [0 1 0])
         (rotate 0.3 [1 0 0])
         (translate [0 0 -150]))))

(defn reposition-thumb-cluster [expr]
  (->> expr
       (rotate (/ 3.14 4) [0 0 1])
       (translate [50 100 -60])))


(defn wall [expr]
  (hull expr
        (->>
         (project expr)
         (extrude-linear {:height 0.0001}))))

(defn vertex-generators [n-rows n-cols n-thumbrows thumb-radius phi radii]
  (let [sphereo (fn [off-x off-y row col]
                  (->> (sphere (* 0.5 case-thickness))
                       (translate [(* switch-plane-size off-x)
                                   (nth radii col)
                                   (* switch-plane-size off-y)])
                       (rotate (* row (row-phi switch-plane-size (nth radii col))) [1 0 0])
                       (reposition col (nth radii col) n-rows)))
        edge-cleaner (fn [top? off-x col]
                       (->> (sphere (* 0.5 case-thickness))
                            (translate [(* switch-plane-size off-x)
                                        (reduce max radii)
                                        (* switch-plane-size -0.5)])
                            (rotate (* (if top? 0 n-rows)
                                       (row-phi switch-plane-size (+ (reduce min radii) 10))) [1 0 0])
                            ;;                    (reposition col (nth radii col) n-rows)
                            (reposition col (reduce max radii) n-rows)))
        shp (fn [off-x off-y c]
              (let [switch-plane-size 18]
                (->> (sphere (* 0.5 case-thickness))
                     ;; (rotate r [ 0  1 0])
                     (translate [ thumb-radius
                                 (* off-x switch-plane-size)
                                 (* off-y switch-plane-size)])
                     (rotate (* c (/ phi n-thumbrows)) [0  1 0])
                     (translate [(- thumb-radius) 0 0])
                     (reposition-thumb-cluster)
                     ;; (rotate (- (* c (/ 3.14 n-keys))) [0  1 0])
                     )))
        last-thumb-btn (fn [off-x off-y]
                         (->>
                          (sphere (* 0.5 case-thickness))
                          (translate [(* (- off-y 1) (- switch-plane-size)) (* off-x switch-plane-size) 5])
                          (reposition-thumb-cluster)
                          (translate [0 0 (* 1.3 switch-plane-size)])))
        first-col-edge (fn [r]
                         (->> (sphereo 1 -0.5 r 0)
                              (translate [0 10 6])))
        last-col-edge (fn [r]
                        (->> (sphereo -1 -0.5 r (dec n-cols))
                             (translate [0 0 13])))
        ]
    {:sphereo sphereo

     :edge-cleaner edge-cleaner
     :shp shp
     :first-col-edge first-col-edge
     :last-col-edge last-col-edge
     :last-thumb-btn last-thumb-btn}))



(defn keyboard [n-rows n-cols n-thumbrows thumb-radius phi radii]
  (let [{sphereo :sphereo 
         edge-cleaner :edge-cleaner 
         shp :shp 
         first-col-edge :first-col-edge 
         last-col-edge :last-col-edge
         last-thumb-btn :last-thumb-btn} (vertex-generators n-rows n-cols n-thumbrows thumb-radius phi radii)
        keyplate (union
                  ;; the keyhole-plates for each row
                  (for [i (range n-cols)
                        j (range n-rows)]
                    (hull
                     (sphereo -0.5 -0.5 j i)
                     (sphereo -0.5 0.5 j i)
                     (sphereo 0.5 -0.5 j i)
                     (sphereo 0.5 0.5 j i)
                     ))
                                        ; the gaps beween two rows
                  (for [i (range (dec n-cols)) ;
                        j (range  n-rows)]
                    (hull
                     (sphereo -0.5 0.5 j i)
                     (sphereo -0.5 -0.5  j i)
                     (sphereo 0.5 -0.5 j (inc i))
                     (sphereo 0.5 0.5  j (inc i))
                     )
                    )
                  ;; for first and last col add 1 more stip and then do hull to bottom projected points
                  
                  (for [r (range (- n-rows 2))]
                    ;; first col
                    (hull
                     (sphereo 0.5 -0.5 r 0)
                     (sphereo 0.5 -0.5 (inc r) 0)
                     (first-col-edge r)
                     (first-col-edge (inc r))
                     ))
                  (for [r (range n-rows)]
                    ;; last col
                    (hull
                     (sphereo 0.5 -0.5 r  n-cols)
                     (sphereo 0.5 -0.5 (inc r) n-cols)
                     (last-col-edge r)
                     (last-col-edge (inc r))
                     )))
        bottom-right-corner (wall
                             (hull
                              (sphereo 0.5 -0.5 n-rows  n-cols)
                              (last-col-edge n-rows)
                              (edge-cleaner false 0.5 n-cols)
                              ))
        top-right-corner (wall
                          (hull
                           (sphereo 0.5  -0.5 0 n-cols)
                           (last-col-edge 0)
                           (edge-cleaner true 0.5  n-cols)
                           ))
        top-left-corner (union
                         (hull
                          (sphereo 0.5 -0.5 0 0)
                          (edge-cleaner true 0.5 0)
                          (first-col-edge 0)
                          )
                         (wall
                          (hull
                           (edge-cleaner true 0.5 0)
                           (first-col-edge 0))))        
        right-wall (for [r (range n-rows)]
                     (union
                      (hull
                       (sphereo -0.5 -0.5 r (dec n-cols))
                       (sphereo -0.5 -0.5 (inc r) (dec n-cols))
                       (last-col-edge r)
                       (last-col-edge (inc r)))
                      (wall
                       (hull
                        (last-col-edge r)
                        (last-col-edge (inc r))))))
        clean-edges (union
                     ;; first row
                     (for [i (range 1 n-cols)]               
                       (union
                        (hull (sphereo -0.5 -0.5 n-rows i)
                              (sphereo 0.5 -0.5 n-rows i)
                              (edge-cleaner false -0.5 i)
                              (edge-cleaner false 0.5 i))
                        (hull (sphereo 0.5 -0.5 n-rows  (inc i))
                              (sphereo -0.5 -0.5 n-rows i)
                              (edge-cleaner false -0.5 i)
                              (edge-cleaner false 0.5  (inc i)))))
                     ;; last row
                     (for [i (range n-cols)]
                       (union
                        (hull (sphereo -0.5 -0.5 0 i)
                              (sphereo 0.5 -0.5 0 i)
                              (edge-cleaner true -0.5 i)
                              (edge-cleaner true 0.5 i))
                        (hull (sphereo -0.5 -0.5 0 i)
                              (sphereo 0.5 -0.5 0 (inc i))
                              (edge-cleaner true -0.5 i)
                              (edge-cleaner true 0.5 (inc i))))))
        front-back-walls (union
                          (for [i (range n-cols)]
                            (union         
                             (wall
                              (hull
                               (edge-cleaner true -0.5 i)
                               (edge-cleaner true 0.5 i)))
                             (wall
                              (hull
                               (edge-cleaner true -0.5 i)
                               (edge-cleaner true 0.5 (inc i))))))
                          (union
                           (for [i (range 1 n-cols)]
                             (union
                              (wall
                               (hull
                                (edge-cleaner false -0.5 i)
                                (edge-cleaner false 0.5 i)))
                              (wall
                               (hull (edge-cleaner false -0.5 i)
                                     (edge-cleaner false 0.5 (inc i))))))))
        key-holes (union
                   (for [i (range n-cols)] ;; the keyholes for each row
                     (->> (switches-col (nth radii i) n-rows)
                          (reposition i (nth radii i) n-rows)
                          ))
                   (->> (switches-col (nth radii 2) -1)
                        (reposition 2 (nth radii 2) -1))
                   (for [c (range n-thumbrows)]
                     (->> (cube (* 2 case-thickness) switch-cuttout-size switch-cuttout-size)
                          (translate [thumb-radius 0 0])
                          (rotate (* c (/ phi n-thumbrows)) [0 1 0])
                          (translate [(- thumb-radius) 0 0])
                          (reposition-thumb-cluster)))
                   (->> (cube  switch-cuttout-size switch-cuttout-size (* 4 case-thickness))
                        (translate [ switch-plane-size 0 0])
                        (reposition-thumb-cluster)
                        (translate [0 0 (* 1.3 switch-plane-size)])))
        key-caps (union
                  (for [i (range n-cols)] ;; the keyholes for each row
                    (->> (caps-col (nth radii i) n-rows)
                         (reposition i (nth radii i) n-rows)
                         ))
                  (for [c (range n-thumbrows)]
                    (->> cap
                         (rotate (* 0.5 Math/PI) [0 1 0])
                         (rotate (/ 3.14 2) [0 0 1])         
                         (translate [thumb-radius 0 0])
                         (rotate (* c (/ phi n-thumbrows)) [0 1 0])
                         (translate [(- thumb-radius) 0 0])
                         (reposition-thumb-cluster)))
                  (->> cap
                       (rotate (/ 3.14 2) [1 0 0])         
                       (translate [ switch-plane-size 0 0])
                       (reposition-thumb-cluster)
                       (translate [0 0 (+ 5 (* 1.3 switch-plane-size))])))
        thumb-cluster (union
                       (for [c (range n-thumbrows)]                         
                         (union (hull
                                 (shp -0.5 -0.5 c)
                                 (shp -0.5 0.5 c)
                                 (shp 0.5 -0.5 c)
                                 (shp 0.5 0.5 c))
                                (hull
                                 (shp -0.5 0.5 (inc c))
                                 (shp 0.5 0.5 (inc c))
                                 (shp -0.5 -0.5 c)
                                 (shp 0.5 -0.5 c))))
                       (hull
                        (shp -0.5 0.5 0)
                        (shp 0.5 0.5 0)
                        (last-thumb-btn -0.5 0.5)
                        (last-thumb-btn 0.5 0.5)
                        )
                       (hull
                        (last-thumb-btn -0.5 0.5)
                        (last-thumb-btn 0.5 0.5)
                        (last-thumb-btn -0.5 -0.5)
                        (last-thumb-btn 0.5 -0.5)))
        bottom-left-corner (union
                            (hull
                             (edge-cleaner false 0.5 1)
                             (sphereo 0.5 -0.5 n-rows 0)
                             (sphereo 0.5 -0.5 n-rows 1))
                            (hull
                             (sphereo 0.5 -0.5 n-rows 0)
                             (sphereo 0.5 -0.5 (dec n-rows) 0)
                             (shp 0.5 -0.5 (dec n-thumbrows))
                             )
                            (hull
                             (edge-cleaner false 0.5 1)
                             (shp -0.5 0.5 0)
                             (last-thumb-btn -0.5 0.5))
                            (hull
                             (sphereo 0.5 -0.5 n-rows 0)
                             (shp 0.5 -0.5 (dec n-thumbrows))
                             (shp -0.5 -0.5 (dec n-thumbrows)))
                            (hull
                             (sphereo 0.5 -0.5 n-rows 0)
                             (sphereo 0.5 -0.5 (dec n-rows) 0)
                             (shp 0.5 -0.5 (dec n-thumbrows)))
                            (hull
                             (sphereo 0.5 -0.5 (dec n-rows) 0)
                             (first-col-edge (- n-rows 2))
                             (shp 0.5 -0.5 (dec n-thumbrows)))
                            (hull
                             (sphereo 0.5 -0.5 (dec n-rows) 0)
                             (first-col-edge (- n-rows 2))
                             (sphereo 0.5 -0.5 (- n-rows 2) 0))
                            (hull
                             (edge-cleaner false 0.5 1)
                             (sphereo 0.5 -0.5 n-rows 0)
                             (shp -0.5 -0.5 (dec n-thumbrows)))
                            (for [i (range  n-thumbrows)]
                              (hull
                               (edge-cleaner false 0.5 1)
                               (shp -0.5 0.5 i)
                               (shp -0.5 0.5 (inc i))))
                            (hull                     
                             (first-col-edge (- n-rows 2))
                             (shp 0.5 -0.5 (- n-thumbrows 2))
                             (shp 0.5 -0.5 (dec n-thumbrows))))
        left-wall (union
                   (for [r (range 1 (dec n-rows))]
                     (wall
                      (hull
                       (first-col-edge r)
                       (first-col-edge (dec r)))))
                   
                   (wall
                    (hull
                     (edge-cleaner false 0.5 1)
                                        ;(shp -0.5 0.5 (- n-thumbrows 3))
                     (last-thumb-btn -0.5 0.5)))
                   (for [b (range (dec n-thumbrows))]
                     (union
                      (wall
                       (hull
                        (shp 0.5 -0.5 b)
                        (shp 0.5 0.5 b)))))
                   (wall
                    (hull
                     (last-thumb-btn 0.5 0.5)
                     (last-thumb-btn 0.5 -0.5)))
                   (wall
                    (hull
                     (shp 0.5 0.5 (- n-thumbrows 1))
                     (first-col-edge (- n-rows 2)))) 
                   (hull
                    (last-thumb-btn -0.5 -0.5)
                    (last-thumb-btn 0.5 -0.5)
                    (->>
                     (last-thumb-btn 0 -0.86)
                     (translate [0 0 13])))
                   (wall
                    (hull
                     (last-thumb-btn -0.5 -0.5)
                     (last-thumb-btn -0.5 0.5)))
                   (wall
                    (hull
                     (shp -0.5 0.5 0)
                     (last-thumb-btn -0.5 0.5)))
                   (wall
                    (hull
                     (shp 0.5 0.5 0)
                     (last-thumb-btn 0.5 0.5)
                     ))
                   (wall
                    (hull
                     (->>
                      (last-thumb-btn 0 -0.86)
                      (translate [0 0 13]))
                     (last-thumb-btn 0.5 -0.5)))
                   (wall
                    (hull
                     (last-thumb-btn -0.5 -0.5)
                     (->>
                      (last-thumb-btn 0 -0.86)
                      (translate [0 0 13])))))
        rj-11-socket-holder (let [pad 2
                                  w 13.5
                                  h 15.5
                                  d 21]
                              (->>
                               (difference
                                (cube (+ d pad) (+ pad w) (+ pad h))
                                (->>
                                 (cube d w h)
                                 (translate [0 pad 0])))
                               (rotate (* -0.5 Math/PI) [1 0 0])
                               (rotate -0.2 [0 0 1])
                               (translate [-35 65(- 35)])))
        rj-11-cuttout (->>
                       (cube 20 15 13)
                       (rotate (* -0.5 Math/PI) [1 0 0])
                       (rotate -0.2 [0 0 1])
                       (translate [-50 68 -39.5]))
        arduino-holder (let [a 36
                             b 21
                             pad 4
                             w (+ a pad)
                             h (+ b pad)]
                         (->>
                          (difference
                           (cube w 9 h)
                           (->>
                            (cube a 13 b)
                            (translate [0 (- pad) 0])))
                          (rotate 0.22 [0 0 1])
                          (translate [-22 97.3 -37.5])))        
        connector-compartment (->>
                               (hull
                                (for [x [0 1]
                                      y [0 1]
                                      z [0 1]]
                                  (->>
                                   (sphere case-thickness)
                                   (translate [(* 20 x) (* 30 y) (if (and (= x 1) (zero? z))
                                                                   12
                                                                   (* 20 z))]))))
                               
                               (rotate -0.2 [0 0 1])
                               (translate [-73 70 -56]))
        arduino-usb-cuttout-wall (intersection
                                  (wall
                                   (hull
                                    (edge-cleaner true -0.5 0)
                                    (edge-cleaner true -0.5 3)
                                    (edge-cleaner false -0.5 0)
                                    (edge-cleaner false -0.5 3)
                                    (first-col-edge 0)))
                                  (->> connector-compartment
                                       (scale [1.4 1.3 1.4])
                                       (translate [22 -24 18])))
        arduino-usb-cuttout (->>
                             (cube 10 10 10)
                             (rotate 0.22 [0 0 1])
                             (translate [-42 92 -39]))
        leg-rest-cuttout (->>
                          (cylinder [100 100] 33300)
                          (rotate (* 0.5 Math/PI) [0 1 0])
                          (rotate (* 0.1 Math/PI) [0 0 1])
                          (translate [0 45 84]))
        ]
    (difference
     (union
      (color [0.5 0.5 0.5 1]
             thumb-cluster
             keyplate
             clean-edges
             top-left-corner
             top-right-corner
             bottom-right-corner
             bottom-left-corner
             front-back-walls
             right-wall
             left-wall
             arduino-holder
             arduino-usb-cuttout-wall
             rj-11-socket-holder
             )
      ;; key-caps
      )
     rj-11-cuttout
     connector-compartment
     key-holes
     arduino-usb-cuttout
     ;; leg-rest-cuttout
     )))

(keyboard 4 7 3 75 (/ 3.14 3.8) [95 95 100 98 80 80 80 80])

;; (with-fn 100
  ;; (keyboard 4 7 3 75 (/ 3.14 3.8) [95 95 95 105 100 80 80 80]))

;; (with-fn 100
  ;; (keyboard 4 6 3 75 (/ 3.14 3.8) [95 95 105 100 80 80 80]))







;; create keyboard parts

;; 4x7 with three cols for pointing fingers
(def right-hand-4x7-3-first-cols
  (keyboard 4 7 3 75 (/ 3.14 3.8) [95 95 95 105 100 80 80 80]))

(def left-hand-4x7-3-first-cols
  (->>
   (keyboard 4 7 3 75 (/ 3.14 3.8) [95 95 95 105 100 80 80 80])
   (mirror [0 1 0])))

(spit "scad_files/right_hand_4x7_3_first_cols.scad"
      right-hand-4x7-3-first-cols)

(spit "scad_files/left_hand_4x7_3_first_cols.scad"
      left-hand-4x7-3-first-cols)



;; 4x7 with three cols for pinky fingers
(def right-hand-4x7-3-last-cols
  (keyboard 4 7 3 75 (/ 3.14 3.8) [95 95 105 100 80 80 80 80]))

(def left-hand-4x7-3-last-cols
  (->>
   (keyboard 4 7 3 75 (/ 3.14 3.8) [95 95 105 100 80 80 80 80])
   (mirror [0 1 0])))

(spit "scad_files/right_hand_4x7_3_last_cols.scad"
      right-hand-4x7-3-last-cols)

(spit "scad_files/left_hand_4x7_3_last_cols.scad"
      left-hand-4x7-3-last-cols)


;; 4x6
(def right-hand-4x6-cols
  (keyboard 4 6 3 75 (/ 3.14 3.8) [ 95 95 105 100 80 80 80]))

(def left-handy-4x6-cols
  (->>
   (keyboard 4 6 3 75 (/ 3.14 3.8) [ 95 95 105 100 80 80 80])
   (mirror [0 1 0])))

(spit "scad_files/right_hand_4x6"
      right-hand-4x6-cols)

(spit "scad_files/left_hand_4x6"
      (->>
       right-hand-4x6-cols
       (mirror [0 1 0])))



