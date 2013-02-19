(ns uk-gridref.core)

(defn to-deg 
  "convert a number to degree"
  [x]
  (/ (* x 180) (. Math PI)))

(defn e-n-to-osgb36
  "convert eastings and northings to osgb takes {:eastings NNN :northings NNNN and returnes {:latitude NN.nnn and :longitude NN.nnn}"
  [gridref]
  (let [E (get gridref :eastings) N (get gridref :northings)
    a 6377563.396 b 6356256.910
    F0 0.9996012717 
    lat0 (/ (* 49 (. Math PI)) 180) 
    lon0 (/ (* -2 (. Math PI)) 180) 
    N0 -100000 
    E0 400000 
    e2 (- 1 (/ (* b b) (* a a)))
    n (/ (- a b) (+ a b))
    lat (loop [lat lat0 M 0]
      (let [
          lat (+ (/ (- N N0 M) (* a F0)) lat)
          Ma (* (+ 1 n (* 5/4 n n) (* 5/4 n n n)) (- lat lat0))
          Mb (* (+ (* 3 n) (* 3 n n) (* 21/8 n n n)) 
            (. Math sin (- lat lat0)) (. Math cos (+ lat lat0)))
          Mc (* (+ (* 15/8 n n) (* 15/8 n n n) ) 
          (. Math sin (* 2 (- lat lat0))) (. Math cos (* 2 (+ lat lon0)))  )
          Md (* 35/24 n n n (. Math sin (* 3 (- lat lat0))) 
          (. Math cos (* 3 (+ lat lat0))))
          M (* b F0 (+ (- Ma Mb) (- Mc Md)))
          ]
        (if (< (- N N0 M) 0.00001)
          (+ (/ (- N N0 M) (* a F0)) lat)
          (recur lat M)
        )))
    coslat (. Math cos lat)
    sinlat (. Math sin lat)
    tanlat (. Math tan lat)
    nu (/ (* a F0) (. Math sqrt (- 1 (* e2 sinlat coslat))))
    rho (/ (* a F0) (. Math pow (- 1 (* e2 sinlat coslat)) 1.5))
    eta2 (- (/ nu rho) 1)
    tan2lat (* tanlat tanlat)
    tan4lat (* tan2lat tan2lat)
    tan6lat (* tan2lat tan4lat)
    seclat (/ 1 coslat)
    nu3 (* nu nu nu)
    nu5 (* nu3 nu nu)
    nu7 (* nu5 nu nu)
    VII (/ tanlat (* rho nu))
    VIII (* (/ tanlat (* 24 rho nu3)) (+ 5 (* 3 tan2lat) eta2 (* -9 tan2lat
    eta2)))
    IX (* (/ tanlat (* 720 rho nu5)) (+ 61 (* 90 tan2lat) (* 45 tan4lat)))
    X (/ seclat nu)
    XI (* (/ seclat (* 6 nu3)) (+ (/ nu rho) (* 2 tan2lat)))
    XII (* (/ seclat (* 120 nu5)) (+ 5 (* 28 tan2lat) (* 24 tan4lat)))
    XIIA (* (/ seclat (* 5040 nu7)) (+ 61 (* 662 tan2lat) (* 1320 tan4lat)
    (* 720 tan6lat)))
    dE (- E E0)
    ]
    (do (println lat))
    {:latitude (to-deg (+ lat (* -1 VII (. Math pow dE 2)) (* VIII (. Math
    pow dE 4)) (* -1 IX (. Math pow dE 6))))
     :longitude (to-deg (+ lon0 (* X dE) (* -1 XI (. Math pow dE 3)) (* XII
     (. Math pow dE 5)) (* -1 XIIA (. Math pow dE 7))))
    }
     
    ))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
