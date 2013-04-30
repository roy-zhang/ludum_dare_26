(ns fun-with-quil.core
  (:require      [fun-with-quil.player :as pl]
                 [fun-with-quil.world  :as wd])
	 (:use quil.core
	       quil.helpers.calc 
	       quil.helpers.drawing
	       quil.helpers.seqs)
	(:gen-class)
 )

(def cfg { :gridSize 10
           :yellow  [255 255 102]
           :brown   [111 79 5]
           
           :round-duration       60000
           
           :start-fade-resources 60000
           :stop-fade-resources  30000
           :resources-fade-to    [255]
           
           :start-fade-trails    30000
           :stop-fade-trails      5000
           :trails-fade-to       [255]
           
           :start-fade-solids    50000
           :stop-fade-solids     25000
           :solids-fade-to       [255]
           
           :start-fade-circles   20000
           :stop-fade-circles    0
           :circles-fade-to      [255]
          })

(defn setup []
  (smooth)
  (background 0)
  (frame-rate 60)
  (stroke 130, 0 0)
  (fill 255 150)
  (ellipse-mode :center)
  (rect-mode :center)
  
  (set-state! :players  [(atom (pl/new-Player 1 [20 20]
                                        [\w \s \a \d]
                                        [0 0 255]
                                           ))
                         (atom (pl/new-Player 2 [30 20]
                                        [\i \k \j \l]
                                        [255 0 0]
                                           ))
                         (atom (pl/new-Player 3 [20 30]
                                       [\8 \5 \4 \6]
                                        [0 255 0]
                                           ))
                         ]
              :world    (atom (wd/new-World (width) (height) (rand-int 200) (rand-int 200) (cfg :gridSize)))
              
              :message  (atom "Click on screen and type a key")
              :lastTime (atom (java.lang.System/currentTimeMillis))
              :alreadyPressed (atom #{})
              :countDown      (atom 60000)
              :stage          (atom 1)
              )
  )

   (defn- reset-round []
     (when (= 1 @(state :stage))
     (dorun  (map reset! (state :players)  
                 [ (pl/new-Player 1 [20 20]
                                  [\w \s \a \d]
                                  [0 0 255]
                                  )
                  (pl/new-Player 2 [10 10]
                                 [\i \k \j \l]
                                 [255 0 0]
                                 )
                  (pl/new-Player 3 [30 30]
                                 [\8 \5 \4 \6]
                                 [0 255 0]
                                 )
                  ]))

(defn next-stage []
 (reset-round)
 (let [nextStage {1 2 2 3 3 4 4 1}]
   (swap! (state :stage) nextStage)))


            
         (reset! (state :world )    (wd/new-World (width) (height) (rand-int 200) (rand-int 200) (cfg :gridSize)))
         (reset! (state :lastTime) (java.lang.System/currentTimeMillis))
         (reset! (state :countDown) (cfg :round-duration))
       ))

;--------------------update
         (defn update-lastTime []
           (let [lastTime  @(state :lastTime) ]
             (- (reset! (state :lastTime) (java.lang.System/currentTimeMillis)) lastTime))
           )
         
         (defn update-countDown [timeSince]
           (if (< @(state :countDown) 0)  (reset! (state :countDown) 0)
             (swap! (state :countDown) - timeSince))
           )
         
         (defn update-players-time [timeSince]
	        (dorun
	          (for [playerAtom (state :players)]
	            (swap! playerAtom pl/move timeSince (quot (width) (cfg :gridSize)) (quot (height) (cfg :gridSize)) )
	      )))
         
         (defn update-players-trails [world]
	        (dorun
	          (for [playerAtom (state :players)]
	            (swap! playerAtom pl/update-trail world )
	      )))
         
 (defn update []
   (-> (update-lastTime)
    (update-countDown)
    (update-players-time))
   
   (swap! (state :world) wd/update (mapv deref (state :players)))
   (update-players-trails @(state :world))
     
   (swap! (state :world) wd/wipe-cmds)
     
     )
  
         
         
;-----------------------draw        

(defn cood [xy]
  (* (cfg :gridSize) xy))

		(defn fullness [timeLeft startFade endFade]
			      (cond 
			        (> timeLeft startFade) 255
			        (< timeLeft endFade)   0
			        :else (* 255 (/  (- timeLeft endFade) (- startFade endFade))
							  )))

		(defn draw-world [fullness]
		  "resources"
          (when (> fullness 0)
		  (no-stroke)
		  (apply fill (cfg :yellow))
		  (dorun
		   (for [[[x y] resource] (wd/good-resources @(state :world))]
		     (rect (cood x) (cood y) resource resource)))
  
		  (apply fill (cfg :brown))
		  (dorun
		   (for [[[x y] resource] (wd/bad-resources @(state :world))]
		     (rect (cood x) (cood y) resource resource)))

			  ))
  
        (defn draw-night [fullness]
          (no-stroke)
          (fill 0 (- 255 fullness))
          (rect (quot (width) 2) (quot (height) 2) (width) (height))
          )
  

		(defn draw-players-trails [fullness]   
		  (no-stroke)
		  (dorun
		   (for [playerAtom (state :players) 
                  :let [[r g b]    (:color @playerAtom)]]
		       (do           (fill r g b fullness)  ; ( stroke r g b) 
		       (dorun 
		          (for [[px py] (:trail @playerAtom)]
		            (rect (cood px) (cood py) 7 7)))
		       ))))
  
        (defn whiten [color fullness]
          (mapv #(+ % (* (- 255 %) (- 1 (/ fullness 255))))
                color)
          )
  
        (defn draw-players-solids [fullness]   
        ;(stroke-weight 3)  
        (no-stroke)
		(let [colors (mapv :color (mapv deref (state :players)))]
		  (dorun   
           (for [ [[x y] id] (filter (fn [[k v]] (not (nil? v) )) (:solid @(state :world)))]
             (do
               ;(apply stroke (conj (vec (colors (dec id))) fullness) )
               (apply fill  (whiten (colors (dec id)) fullness) )
               (rect (cood x) (cood y) (cfg :gridSize) (cfg :gridSize)))) 
           )))
            
        (defn draw-players-circles [fullness]
  		  (no-fill)   
          (stroke-weight 4)
			  (dorun
			   (for [playerAtom (state :players)]
			     (let [[x y] (:pos @playerAtom)
			           rad   (max 3 (pl/total-energy @playerAtom) )]
			       (apply stroke (conj (vec (:color @playerAtom)) fullness))
			       (ellipse (cood x) (cood y) (* 5 rad) (* 5 rad))            
			     ))))
  
		

(defn draw [] 
  (background 255)
  
    (when (= 1 @(state :stage))
         (fill 0)
         (text (str "Rules: Your goal is to claim as many yellow squares as possible" ) 20 80)
         (text (str "while avoiding as many brown squares as possible" ) 60 100)
         (text (str "To claim an area, create a trail surrounding it" ) 60 120)
         (text (str "controls:  Blue   w s a d" ) 20 160)
         (text (str "           Red    i k j l" ) 40 180)
         (text (str "           Green  8 5 4 6" ) 40 200)
         (text (str "Round lasts for one minute   -   click to continue" ) 20 240)
         
      )
    (when (= 2 @(state :stage))
        (update) 
	    (draw-world           (fullness @(state :countDown) (cfg :start-fade-resources) (cfg :stop-fade-resources)))
	    (draw-night           (fullness @(state :countDown) (cfg :start-fade-resources) (cfg :stop-fade-resources)))
	    (draw-players-trails  (fullness @(state :countDown) (cfg :start-fade-resources) (cfg :stop-fade-resources)))
	    (draw-players-solids  (fullness @(state :countDown) (cfg :start-fade-solids)    (cfg :stop-fade-solids)))   
	    (draw-players-circles (fullness @(state :countDown) (cfg :start-fade-circles)   (cfg :stop-fade-circles)))
     
        (when ( = 0 @(state :countDown))
          (next-stage))
    )
    
     (when (= 3 @(state :stage))
       (let [cx (quot (width) 2)
             cy (quot (width) 4)]
         (fill 255)
         (ellipse cx cy 100 100)
         (fill 0)
         (text (str "~ Round Over ~") (- cx 40) cy)
         (text (str "click to continue") (- cx 50) (+ 10 cy))
      ))
    
    (when (= 4 @(state :stage))
     (let [cx (quot (width) 2)
           cy (quot (width) 4)
           scores (wd/score-players @(state :stage))]
      (draw-world 255)
      (draw-players-solids 150)
      
      (fill 0)
      (rect cx cy 75 75)
      (fill 255)
       (text (str "Blue: "  (scores 1)) (- cx 40) (-30 cy))
       (text (str "Red: "   (scores 2))  (- cx 40)  (- 10 cy))
       (text (str "Green: " (scores 3))  (- cx 40)  (+ 10 cy))
      
      
      ))

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

(defn next-stage []
 (reset-round)
 (let [nextStage {1 2 2 3 3 4 4 1}]
   (swap! (state :stage) nextStage)))


(defn -main [& args]
	(defsketch ludum-dare-26
	  :title "tuber tussle"
	  :setup setup
	  :size [1000 1000]
	  :key-pressed key-press
	  :key-released key-release
      :mouse-released next-stage
      :mouse-e
	  :draw draw)
)
