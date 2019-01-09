(ns smue-keyboard.core
(:refer-clojure :exclude [use import])
(:require [scad-clj.scad :refer :all]
          [scad-clj.model :refer :all]
          [smue-keyboard.utils :refer :all]))




(def keyboard-specs
  {:fingers [{:r 95 :n 5 :off-x 0 :off-y 0 :rot-y -1.15}
             {:r 95 :n 5 :off-x 22 :off-y 0 :rot-y 0}
             {:r 98 :n 5 :off-x 22 :off-y 0.5 :rot-y 0}
             {:r 96 :n 5 :off-x 22 :off-y 0.3 :rot-y 0}
             {:r 89 :n 5 :off-x 0 :off-y 0 :rot-y 0}
             {:r 89 :n 5 :off-x 0 :off-y 0 :rot-y 1.2}]
   
   :thumb [[{:col 0 :row 0 :off-y -0.5}
            {:col 0 :row 1 :off-y -0.5}
            {:col 0 :row 2 :off-y -0.5}]
           [{:col 1.3 :row 0 :off-y 0}
            {:col 1.3 :row 1 :off-y 0}]]})


(defn finger-vtxs [specs]
  (let [a (* 0.5 switch-plane-size)
        a- (- a)
        h (+ 22 (reduce max (map :r (:fingers specs))))]
    (loop [cols (:fingers specs)
           vtxs '() 
           x 0]
      (if (empty? cols)
        (vec (reverse vtxs))
        (let [col (first cols)
              {n :n 
               r :r
               off-x :off-x
               off-y :off-y
               rot-y :rot-y} col
              phi (row-phi switch-plane-size r)
              phi0 (* phi n -0.5)
              pts (for [i (range n)]
                    {:bl [a- a- 0]
                     :tl [a- a 0]
                     :br [a a- 0]
                     :tr [a a 0]})
              pts (for [i (range n)
                        :let [plate (nth pts i)]]
                    (update-values
                     plate
                     (fn [p]
                       (-> p
                           (translate-v [0 0 (- r)])
                           (rotate-v :y (* rot-y (- phi)))
                           (rotate-v :x (+ (* phi i (if-not (zero? rot-y) 1.05 1))
                                           (* phi off-y)
                                           phi0))
                           (translate-v [x 0 h])
                           (rotate-v :y 0.3)
                           (rotate-v :x 0.1)))))]
          (recur (rest cols) (conj vtxs pts) (+ x off-x)))))))



(def vtxs (finger-vtxs keyboard-specs))



(defn thumb-vtxs [specs]
  (let [cols (:thumb specs)
        b (* 0.5 (inc switch-plane-size))
        r 50
        plates (vec
                (for [col cols]
                  (vec
                   (for [p col]
                     (update-values
                      {:bl [(- b) (- b) 0]
                       :tl [(- b) b 0]
                       :br [b (- b) 0]
                       :tr [b b 0]}
                      #(-> %
                           (translate-v [0 0 (- r)])
                           (rotate-v :x (* (+ (:off-y p) (:row p))
                                           (row-phi switch-plane-size r)))
                           (rotate-v :y (* -1 (:col p) (row-phi switch-plane-size r)))
                           (translate-v [0 0 r])
                           (rotate-v :x -0.1)
                           (rotate-v :y 0.3)
                           (rotate-v :z -0.9)
                           ;; (rotate-v :y -0.4)
                           (translate-v [-62
                                         -35 28])))))))]
    plates))



(def t-vtxs (thumb-vtxs keyboard-specs))



(defn  keyplate [vtxs]
  (union
   (for [col vtxs
         p col]
     (hull
      (sp (:br p))
      (sp (:tl p))
      (sp (:bl p))
      (sp (:tr p))))
   (loop [cols vtxs
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
                (sp (:bl pb))
                (sp (:tl pb))
                (sp (:br pa))
                (sp (:tr pa)))))
           acc)))))))


(defn walls [vtxs starts]
  (let [[r l t b] starts
        waller (fn [col start from-key to-key]
                 (let [plates (drop start col)]
                   (union
                    (for [plate plates]
                      (wall (hull (sp (from-key plate))
                                  (sp (to-key plate)))))
                    (loop [ps plates
                           acc '()]
                      (if (>= 1 (count ps))
                        acc
                        (recur (rest ps)
                               (conj acc
                                     (union
                                      (wall (hull (sp (to-key (first ps)))
                                                  (sp (from-key (second ps)))))))))))))
        wall-right (waller (last vtxs) r :br :tr)
        wall-top (waller (map last vtxs)  t :tl :tr)
        wall-bottom (waller (map first vtxs) b :bl :br)
        wall-left (waller (first vtxs) l :bl :tl)]
    (union wall-right
           wall-left
           wall-top
           wall-bottom)))

;(walls vtxs [0 3 0 0])


(defn switch-cuttout [p]
  (let [a (* 0.5 switch-plane-size)
        a- (* -0.5 switch-plane-size)
        b (* 0.5 switch-cuttout-size)
        b- (* -0.5 switch-cuttout-size)
        h 3
        h- (- h)
        y (normalize-v (mapv - (:tl p) (:bl p)))
        x (normalize-v (mapv - (:br p) (:bl p)))
        m (mean-point p)
        n (normalize-v (cross-p y x))
        v (fn [a b c]
            (-> m
                (translate-v (scale-v x a))
                (translate-v (scale-v y b))
                (translate-v (scale-v n c))))
        verts [(v b- b- h-)
               (v b b- h-)
               (v b b h-)
               (v b- b h-)
               (v b- b- h)
               (v b b- h)
               (v b b h)
               (v b- b h)]
        faces [[0 1 2 3]
               [4 5 1 0]
               [7 6 5 4]
               [5 6 2 1]
               [6 7 3 2]
               [7 4 0 3]]
        faces (mapv #(vec (reverse %)) faces)]
    (polyhedron verts faces)))

(defn switchplate-cleaner [p]
  (let [
        b (* 0.5 switch-plane-size)
        b- (- b)
        h 3
        h- (- h)
        y (normalize-v (mapv - (:tl p) (:bl p)))
        x (normalize-v (mapv - (:br p) (:bl p)))
        m (mean-point p)
        n (normalize-v (cross-p x y))
        v (fn [a b c]
            (-> m
                (translate-v (scale-v x a))
                (translate-v (scale-v y b))
                (translate-v (scale-v n c))))
        verts [(v b- b- 2)
               (v b b- 2)
               (v b b 2)
               (v b- b 2)
               (v b- b- 14)
               (v b b- 14)
               (v b b 14)
               (v b- b 14)]
        faces [[0 1 2 3]
               [4 5 1 0]
               [7 6 5 4]
               [5 6 2 1]
               [6 7 3 2]
               [7 4 0 3]]
       ; faces (mapv #(vec (reverse %)) faces)
        ]
    (polyhedron verts faces)))



(defn key-cuttouts [vtxs]
  (union
   (for [col vtxs
         plate col]
     (switch-cuttout plate))))



(defn thumb-cluster []
  (union
   (keyplate t-vtxs)
   (walls t-vtxs [0 0 2 0])))


(defn stitch-together [c1 c2]
  (let [[ps1 ps2] (if (< (count c1) (count c2)) [c2 c1] [c1 c2])]
    (loop [faces []
           f1 ps1 
           f2 ps2]
      (if (and (= 1 (count f1)) (= 1 (count f2)))
        (union faces)
        (let [p1 (first f1)
              p2 (second f1)
              p3 (first f2)
              face (hull (sp p1) (sp p2) (sp p3))
              r1 (rest f1)
              r2 (if (= (count f2) 1) f2 (rest f2))]
          (if (>= (count f2) 2)
            (let [p4 (second f1)
                  p5 (second f2)
                  p6 (first f2)
                  face2 (hull (sp p4) (sp p5) (sp p6))]
              (recur (-> faces (conj face) (conj face2)) r1 r2))
            (recur (conj faces face) r1 r2)))))))


(defn patch [vtxs t-vtxs]
  (let [c1 (drop-last 2 (first vtxs))
        c2 (reverse (map last t-vtxs))]
    (union
      (stitch-together
                (interleave (mapv :bl c1) (mapv :tl c1))
                (interleave (mapv :tr c2) (mapv :tl c2)))
     (wall (hull (-> c1 first :bl sp)
                 (-> c2 first :tr sp)))
     (wall (hull (-> c1 last :tl sp)
                 (-> c2 last :tl sp))))))



(defn rj-11-socket-holder [vtxs]
  (let [pad 2
        w 13.5
        h 15.5
        d 21
        [bx by bz] (-> vtxs first drop-last last :tl)
        ]
    (->>
     (difference
      (cube (+ d pad) (+ pad w) (+ pad pad h))
      (->>
       (cube d w h)
       (translate [0 -1 0])))
     ;(rotate (* -0.5 Math/PI) [1 0 0])
     (translate [(+ bx pad pad (* 0.5 w))
                 (- by 5)
                 (* (+ pad pad h) 0.5)]))))

(defn rj-11-cuttout [vtxs]
  (let [[x y z] (-> vtxs first drop-last last :tl)]
    (->>
     (cube 20 15 13)
     ;(rotate (* -0.5 Math/PI) [1 0 0])
     (translate  [x (- y 7.5) 10]))))

(defn arduino-holder [vtxs]
  (let [a 36
        b 21
        pad 4
        w (+ a pad)
        h (+ b pad)
        [x y z] (-> vtxs first last :tr)]
    (->>
     (difference
      (cube w 9 h)
      (->>
       (cube a 13 b)
       (translate [0 (- pad) 0])))   
     (translate [(+ 2 x) (- y case-thickness 3) (* h 0.5)]))))        

(defn arduino-usb-cuttout [vtxs]
  (let [[x y z]  (-> vtxs first last :tl)]
    (->>
     (cube 10 8 15)
          (translate [x (- y 5) 12.5]))))


(defn keyboard [vtxs]  
  (difference
   (union
    (keyplate vtxs)
    (walls vtxs [0 3 0 0])
    (thumb-cluster)
    (patch vtxs t-vtxs)
    (rj-11-socket-holder vtxs)
    (arduino-holder vtxs))
   (switchplate-cleaner (-> t-vtxs last last))
   (key-cuttouts t-vtxs)
   (key-cuttouts vtxs)
   (rj-11-cuttout vtxs)
   (arduino-usb-cuttout vtxs)))


(keyboard vtxs)

(def right-hand
  (keyboard vtxs))

(def left-hand
  (->>
   (keyboard vtxs)
   (mirror [0 1 0])))

(spit "scad_files/right_hand.scad"
      (write-scad right-hand))

(spit "scad_files/left_hand.scad"
      (write-scad left-hand))
