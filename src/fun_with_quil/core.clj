(ns fun-with-quil.core
  (:require      [fun-with-quil.player :as pl]
                 [fun-with-quil.world  :as wd])
 (:use quil.core
       quil.helpers.calc 
       quil.helpers.drawing
       quil.helpers.seqs)
 )

(def cfg { :gridSize 10
           :yellow  [255 255 102]
           :brown   [111 79 5]
          })


(defn setup []
  (smooth)
  (background 0)
  (frame-rate 60)
  (stroke 130, 0 0)
  (fill 255 150)
  (ellipse-mode :center)
  (rect-mode :center)
  
  (set-state! :players  [(atom (pl/new-Player [20 20]
                                        [\w \s \a \d]
                                        [0 0 255]
                                           ))
                         (atom (pl/new-Player [10 10]
                                        [\i \k \j \l]
                                        [255 0 0]
                                           ))
                         ]
              :world    (atom (wd/new-World (width) (height) (rand-int 200) (rand-int 200) (cfg :gridSize)))
              
              :message  (atom "Click on screen and type a key")
              :lastTime (atom (java.lang.System/currentTimeMillis))
              :alreadyPressed (atom #{})
              :countDown      (atom 30000)
              )
  )


;--------------------update
         (defn update-lastTime []
           (let [lastTime  @(state :lastTime) ]
             (- (reset! (state :lastTime) (java.lang.System/currentTimeMillis)) lastTime))
           )
         
         (defn update-countDown [timeSince]
           (if (< @(state :countDown) 0)  (reset! (state :countDown) 0)
             (swap! (state :countDown) - timeSince)
           ))
         
         (defn update-players [timeSince]
	        (dorun
	          (for [playerAtom (state :players)]
	            (swap! playerAtom pl/move timeSince (quot (width) (cfg :gridSize)) (quot (height) (cfg :gridSize)) )
	      )))
         
         (defn update []
           (let [timeSince (update-lastTime)]
	        (update-countDown timeSince)
            (update-players timeSince))          
           )
         
         
;-----------------------draw        
(defn draw-grid []
  "grid of 10 x 10"
  (stroke 0 0 0 10)
  (fill 0 0 0)
  (dorun
    (for [y (range-incl 5 (height) (cfg :gridSize))
          x (range-incl 5 (width) (cfg :gridSize))]
      (rect x y 10 10))))


(defn draw-players []
   
  (no-fill)   
       
   ;trail     
  (stroke-weight 1)
  (dorun
   (for [playerAtom (state :players)]
       (do           (apply stroke (:color @playerAtom))  
       (dorun 
          (for [[px py] (:trail @playerAtom)]
            (rect (* (cfg :gridSize) px) (* (cfg :gridSize) py) (cfg :gridSize) (cfg :gridSize))))
       )))
        
        
   ;circles
   (stroke-weight 4)
	  (dorun
	   (for [playerAtom (state :players)]
	     (let [[x y] (:pos @playerAtom)
	           rad   (max 3 (pl/total-energy @playerAtom) )]
	       (apply stroke (:color @playerAtom))
	    
	       (ellipse (* x (cfg :gridSize)) (* y (cfg :gridSize)) (* 5 rad) (* 5 rad))            
	     )))
          
   (stroke-weight 1)
  )
  

(defn draw-world []
  ;(stroke-weight 1)
  (no-stroke)
  (apply fill (cfg :yellow))
  (dorun
   (for [[[x y] resource] (wd/good-resources @(state :world))]
     (rect x y resource resource)))
  
  (apply fill (cfg :brown))
  (dorun
   (for [[[x y] resource] (wd/bad-resources @(state :world))]
     (rect x y resource resource)))

  )                 

(defn draw []
    
   ;update
    (update)
   
    (background 255)
    ;(draw-squares 5 5)
    (draw-world)
    ;(draw-grid)
    (draw-players)
    
    (stroke 10 10 10)
    (reset! (state :message) (str "last key: "  (key-code)  (:release @(first (state :players)))   ))
    (text @(state :message) 20 60)
    (text (str "time left: " (quot @(state :countDown) 1000)) 20 80)
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

(defsketch ludum-dare-26
  :title "tuber tussle"
  :setup setup
  :size [1000 1000]
  :key-pressed key-press
  :key-released key-release
  :draw draw)