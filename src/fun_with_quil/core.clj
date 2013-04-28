(ns fun-with-quil.core
  (:require      [fun-with-quil.player :as pl])
 (:use quil.core
       quil.helpers.calc 
       quil.helpers.drawing
       quil.helpers.seqs)
 )


(defn setup []
  (smooth)
  (background 0)
  (frame-rate 60)
  (stroke 130, 0 0)
  (fill 255 150)
  
  (set-state! :players  [(atom (pl/new-Player [20 20]
                                        [\w \s \a \d]
                                           ))]
              :message  (atom "Click on screen and type a key")
              :lastTime (atom (java.lang.System/currentTimeMillis))
              :alreadyPressed (atom #{})
              )
  )

(defn draw-grid []
  "grid of 10 x 10"
  (dorun
    (for [y (range-incl 5 (height) 10)
          x (range-incl 5 (width) 10)]
      (rect x y 10 10))))

(defn pos-to-cood [[px py]] 
   "* 10"  [ ])

(defn draw-player []
  (dorun
   (for [playerAtom (state :players)]
     (let [[x y] (:pos @playerAtom)]
       (ellipse (* x 10) (* y 10) 20 20)     
     )
  )))
  
				   (defn draw-point
				  [x y noise-factor]
				  (let [len (* 10 noise-factor)]
				    (rect x y len len)))
				
				(defn draw-squares
				  [x-start y-start]
				
				  (dorun
				   (for [y (range-incl 0 (height) 10)
				         x (range-incl 0 (width) 10)]
				     (let [x-noise (mul-add x 0.01 x-start)
				           y-noise (mul-add y 0.01 y-start)
				           alph    (* 255 (noise x-noise y-noise))]
				       (draw-point x y (noise x-noise y-noise))))))
    
    
                 (defn update-lastTime []
                   (let [lastTime  @(state :lastTime) ]
                     (- (reset! (state :lastTime) (java.lang.System/currentTimeMillis)) lastTime))
                   )
                 

(defn draw []
    
   ;update
    (let [timeSince (update-lastTime)]
        (dorun
      (for [playerAtom (state :players)]
        (swap! playerAtom pl/move timeSince (width) (height))
      )))

   
    (background 0)
   ; (draw-squares 5 5)
    ;(draw-grid)
    (draw-player)
    
    (reset! (state :message) (str "last key: "   (:release @(first (state :players)))   ))
    (text @(state :message) 20 60)
    
  )


(defn key-press []
  (if-not (contains? @(state :alreadyPressed) (raw-key))
    (do 
      (swap! (state :alreadyPressed) conj (raw-key))
      (dorun
        (map #(swap! % pl/key-press (raw-key))
           (state :players))))))

(defn key-release []
  (swap! (state :alreadyPressed) disj (raw-key))
    (dorun
  (map #(swap! % pl/key-release (raw-key))
    (state :players))))

(defsketch gen-art-21
  :title "tuber tussle"
  :setup setup
  :size [1000 1000]
  :key-pressed key-press
  :key-released key-release
  :draw draw)