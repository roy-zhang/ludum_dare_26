(ns fun-with-quil.world
 (:use quil.core
       quil.helpers.calc 
       quil.helpers.drawing
       quil.helpers.seqs)
  )


(def cfg { :range 20
           :cut 10
             })

(defrecord World [resources])


(defn- noiseFn [[x y] randX randY]
  (* (cfg :range)
     (noise (mul-add x 0.01 randX) (mul-add y 0.01 randY))))

(defn new-World [width height randX randY gridSize]
  (let [resources
        
        (loop [coods (for [y (range-incl 0 height  gridSize) x (range-incl 0 width gridSize)]  [x y]) 
               resources  (transient {})]
              (if-not (empty? coods)
                 (recur (rest coods) 
                        (assoc! resources (first coods) (noiseFn (first coods) randX randY)))
                 (persistent! resources)))
        ]
    
    (->World resources)
    ))

(defn good-resources [world]
  (apply merge
	  (for [ [cood resource] (filter (fn [[c r]] (> r (cfg :cut))) (:resources world)) ]
        {cood (- resource (cfg :cut))}	    )))

(defn bad-resources [world]
  (apply merge
	  (for [ [cood resource] (filter (fn [[c r]] (< r (cfg :cut))) (:resources world))       ]
        {cood (- (cfg :cut) resource)}	    )))