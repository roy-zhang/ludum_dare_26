(ns fun-with-quil.world
  (:require      [fun-with-quil.player :as pl])
 (:use quil.core
       quil.helpers.calc 
       quil.helpers.drawing
       quil.helpers.seqs)
  )


(def cfg { :range 20
           :cut 10
             })

(defrecord World [resources playersGrid solid commandSet])


(defn- noiseFn [[x y] randX randY]
  (* (cfg :range)
     (noise (mul-add x 0.01 randX) (mul-add y 0.01 randY))))

(defn new-World [width height randX randY gridSize]
  (let [resources  (apply merge 
                     (for [y (range-incl 0 height  gridSize) 
                           x (range-incl 0 width   gridSize)]
                       {[(quot x gridSize) (quot y gridSize)] (noiseFn [x y] randX randY)}))
        
        emptyMap  (apply merge 
                     (for [y (range-incl 0 height  gridSize) 
                           x (range-incl 0 width   gridSize)]
                       {[(quot x gridSize) (quot y gridSize)] nil}))
        ]
    (->World resources emptyMap emptyMap #{})
    ))



;---update


(defn update-grid [world players]
  "update playergrid, or add cut to cmds"
    (let [update-world 
          (fn [world player]
            (reduce (fn [world step]  
                      (if-let [current ((:playersGrid world) step)]
                          (if (= current (:id player))
                            (-> world
                              (update-in  [:commandSet] conj [:box current step])
                              (update-in  [:commandSet] conj [:cut current step]))
                            (update-in world [:commandSet] conj [:cut current step])
                          )
                        (assoc-in world [:playersGrid step] (:id player))))
                    world (reverse (:lastSeenByWorld player))) )
          ]
      (reduce update-world (assoc world :commandSet #{}) players))
  )


	(defn- wipeout [world player step]
	  (let [suspects (drop-while #(not= %  step) (concat (:lastSeenByWorld player) (:trail player)))
	        id       (:id player)]
	    (reduce (fn [world step]
	              (if (and (nil? ((:solid world) step)) (= id ((:playersGrid world) step)))
	                (assoc-in world [:playersGrid step] nil)
	                world                       
	              ))
	              world
	              suspects)))
 
(defn update-trails [world players]
  "playersgrid remove cut off"
  (reduce (fn [world [type victim place]] 
	        (case type
	          :cut (wipeout world (nth players (dec victim)) place)
               world
              ))
          world
         (:commandSet world))
    )

    (defn- flooded [bounds]
      "still need to check when going out of bounds, stops at negatives or greater than width/height"
      (let [someMidPoint (mapv #(quot % 2) (mapv + (first bounds) (nth bounds (quot (count bounds) 2))))
            notInBoundsOrFound  (fn [found pos] 
                               (not (or (contains? found pos) 
                                        (contains? (set bounds) pos))))]
      (loop [ [top & rest] (list someMidPoint)
              found  #{}  ]
        (if (< (count rest) 400)
	        (if top
		       (if (notInBoundsOrFound found top)
		           (let [ up (mapv + top [0 -1]) 
		                  dw (mapv + top [0 1])
		                  lf (mapv + top [-1 0])
		                  rt (mapv + top [1 0])]
				          (recur 
					          (cond-> rest
					             (notInBoundsOrFound found up)   (conj up)
					             (notInBoundsOrFound found dw)   (conj dw)
					             (notInBoundsOrFound found lf)   (conj lf)
					             (notInBoundsOrFound found rt)   (conj rt)
					                )
				              (conj found top)) )
		          (recur rest found))
	           found)
         #{}) )
      ))
          

	(defn- boxout [world player step]
	  (let [suspects (take-while #(not= %  step) (rest (drop-while #(not= %  step) (concat (:lastSeenByWorld player) (:trail player)))))
	        id       (:id player)]
         (if (every? #(= id ((:playersGrid world) %)) suspects)
           (reduce (fn [world step]
                      (assoc-in world [:solid step] id))
                    world
                    (flooded suspects))   
            world)))


 (defn update-boxes [world players]
  (reduce (fn [world [type victim place]] 
	        (case type
	          :box (boxout world (nth players (dec victim)) place)
               world
              ))
          world
         (:commandSet world))
    )

 (defn update [world players]
  (-> world
    (update-grid players)
    (update-boxes players)
    (update-trails players)

  ))
 
(defn wipe-cmds [world]
  (assoc world :commandSet #{}))
  

(defn score-players [world]
  (let [grouped (group-by second (:solid world))]
     (->> (dissoc grouped nil) 
       (map (fn [[id bag]] 
              { id  (->> bag
                      (map first)
                      (map (:resources world))
                      (map #(- % (cfg :cut)))
                      (apply +))               }))   
       (apply merge)
   )))
	  


(defn good-resources [world]
  (apply merge
	  (for [ [cood resource] (filter (fn [[c r]] (> r (cfg :cut))) (:resources world)) ]
        {cood (- resource (cfg :cut))}	    )))

(defn bad-resources [world]
  (apply merge
	  (for [ [cood resource] (filter (fn [[c r]] (< r (cfg :cut))) (:resources world))       ]
        {cood (- (cfg :cut) resource)}	    )))