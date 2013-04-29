(ns fun-with-quil.player)

(defrecord Player [id pos keys color energy release commandQueue trail lastSeenByWorld])

(def cfg { :compression 10
              :milliPerPos 2
             })

(defn now [] (java.lang.System/currentTimeMillis))

(defn new-Player [id startPos keys color]
    (->Player id
              startPos 
              (apply hash-map (interleave keys [:u :d :l :r]))
              color
              {:u nil :d nil :l nil :r nil}
              {:u 0 :d 0 :l 0 :r 0}
              clojure.lang.PersistentQueue/EMPTY
              '()
              nil
              ))
  

(defn key-press [player key]
  "records starttime in energy"
  (if-let [dir ((:keys player ) key)]
    (assoc-in player [:energy dir] (now))
    player))

(defn key-release [player key]
  "records release"
	  (if-let [dir ((:keys player) key)]
	    (-> player
	      (assoc-in   [:release dir] (-> player 
	                                   (get-in [:energy dir]) 
	                                   (- (now))
	                                   (- )
	                                   (quot (cfg :compression))))
	      (assoc-in [:energy dir] nil)
	      (update-in  [:commandQueue] conj dir)
	    )
	    player))

(defn- move-trail [player oldx oldy dir]
  (let [[cx cy] (:pos player)
        path    (case dir
			      :u (map #(vector oldx %)  (range cy oldy))
			      :d (map #(vector oldx %) (reverse (range (inc oldy) (inc cy))))
			      :l (map #(vector % oldy)  (range cx oldx))
			      :r (map #(vector % oldy) (reverse (range (inc oldx) (inc cx))))
			      )
        ]
    (assoc-in player [:lastSeenByWorld] path)
	      ))

(defn- move-pos [player dir milli width height]
  (let [posMoved (quot milli (cfg :milliPerPos))
        [cx cy]  (:pos player)]
 (case dir
	      :u (-> player
				(assoc :pos [ cx  (max 0 (- cy posMoved))])
				(move-trail cx cy :u))
	      :d (-> player
				(assoc :pos [ cx  (min height (+ cy posMoved))]) 
				(move-trail cx cy :d))
	      :l (-> player
	            (assoc :pos [ (max 0 (- cx posMoved)) cy])
	            (move-trail cx cy :l))
	      :r (-> player
	            (assoc :pos [ (min width (+ cx posMoved)) cy])
	            (move-trail cx cy :r))
	      )
     ))

(defn move [player timeSince width height]
  "based on release and time since, adjusts position of player"
  (if-let [dir (peek (:commandQueue player))]
    (let [release ((:release player) dir)]
      (if (< timeSince release)
        (-> player
          (update-in [:release dir] - timeSince)
          (move-pos  dir timeSince width height))
        (-> player
          (update-in [:commandQueue] pop)
          (move-pos  dir release width height)
          (assoc-in [:release dir] 0)
          )))
    player))

(defn update-trail  [player world]
  (let [
        relevantCmds (filter (fn [[cmd victim cood]] (= (:id player) victim)) (:commandSet world))
        newTrail     (reduce  (fn [trail cmd] 
						        (case (first cmd)
						          :cut (take-while #(not= %  (last cmd)) trail)
					              )) (concat (:lastSeenByWorld player) (:trail player)) relevantCmds)
        ]
    (-> player
      (assoc :trail newTrail)
      (assoc :lastSeenByWorld '() ))))

(defn total-energy [player]
   (quot
      (->> player
      (:energy)
      (vals)
      (filter #(not (nil? %)))
      (map #(- (now) %) )
      (apply +)
      )
       (* (cfg :compression) (cfg :milliPerPos))
    ))
    