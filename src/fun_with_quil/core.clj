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
           :stop-fade-trails     10000
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
                         (atom (pl/new-Player 2 [10 10]
                                        [\i \k \j \l]
                                        [255 0 0]
                                           ))
                         (atom (pl/new-Player 3 [30 30]
                                       [\8 \5 \4 \6]
                                        [0 255 0]
                                           ))
                         ]
              :world    (atom (wd/new-World (width) (height) (rand-int 200) (rand-int 200) (cfg :gridSize)))
              
              :message  (atom "Click on screen and type a key")
              :lastTime (atom (java.lang.System/currentTimeMillis))
              :alreadyPressed (atom #{})
              :countDown      (atom 60000)
              :stage          1
              )
  )
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
    (update)
    
    (background 255)
    (draw-world           (fullness @(state :countDown) (cfg :start-fade-resources) (cfg :stop-fade-resources)))
    (draw-night           (fullness @(state :countDown) (cfg :start-fade-resources) (cfg :stop-fade-resources)))
    (draw-players-trails  (fullness @(state :countDown) (cfg :start-fade-resources) (cfg :stop-fade-resources)))
    (draw-players-solids  (fullness @(state :countDown) (cfg :start-fade-solids)    (cfg :stop-fade-solids)))   
    (draw-players-circles (fullness @(state :countDown) (cfg :start-fade-circles)   (cfg :stop-fade-circles)))
    

   ; (text (str "scores" (mapv (partial wd/score-player @(state :world)) (mapv deref (state :players))) 20 60))
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



(defn mouse-pressed []
  
  
  
  )


(defsketch ludum-dare-26
  :title "tuber tussle"
  :setup setup
  :size [1000 1000]
  :key-pressed key-press
  :key-released key-release
  :draw draw)


(defn -main [& args]
	(defsketch ludum-dare-26
	  :title "tuber tussle"
	  :setup setup
	  :size [1000 1000]
	  :key-pressed key-press
	  :key-released key-release
      :mouse-pressed mouse-pressed
	  :draw draw)
)
