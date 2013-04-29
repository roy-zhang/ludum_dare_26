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


(defn wipeout [world player step]
  (let [suspects (drop-while #(not= %  step) (concat (:lastSeenByWorld player) (:trail player)))
        id       (:id player)]
    (reduce (fn [world step]
              (if (and (nil? ((:solid world) step)) (= id ((:playersGrid world) step)))
                (assoc-in world [:playersGrid step] nil)
                world                       
              ))
              world
              suspects)))

(defn update [world players]
    (let [update-world 
          (fn [world player]
            (reduce (fn [world step]  
                      (if-let [current ((:playersGrid world) step)]
                        (-> world 
                          (wipeout player step)
                          (update-in [:commandSet] conj [:cut current step])
                          )
                        (assoc-in world [:playersGrid step] (:id player))))
                    world (reverse (:lastSeenByWorld player))) )
          ]
      (reduce update-world (assoc world :commandSet #{}) players))
  )
  

(defn good-resources [world]
  (apply merge
	  (for [ [cood resource] (filter (fn [[c r]] (> r (cfg :cut))) (:resources world)) ]
        {cood (- resource (cfg :cut))}	    )))

(defn bad-resources [world]
  (apply merge
	  (for [ [cood resource] (filter (fn [[c r]] (< r (cfg :cut))) (:resources world))       ]
        {cood (- (cfg :cut) resource)}	    )))