(ns fun-with-quil.player)

(defrecord Player [pos keys energy release commandQueue])

(def config { :compression 10
              :milliPerPos 5
             })

(defn new-Player [startPos keys]
    (->Player  startPos 
              (apply hash-map (interleave keys [:u :d :l :r]))
              {:u 0 :d 0 :l 0 :r 0}
              {:u 0 :d 0 :l 0 :r 0}
              clojure.lang.PersistentQueue/EMPTY
              ))
  

(defn key-press [player key]
  "records starttime in energy"
  (if-let [dir ((:keys player ) key)]
    (assoc-in player [:energy dir] (java.lang.System/currentTimeMillis))
    player))

(defn key-release [player key]
  "records release"
  (if-let [dir ((:keys player) key)]
    (-> player
      (assoc-in   [:release dir] (-> player 
                                   (get-in [:energy dir]) 
                                   (- (java.lang.System/currentTimeMillis))
                                   (- )
                                   (quot (config :compression))))
      (update-in  [:commandQueue] conj dir) 
    )
    player))

(defn- move-pos [player dir milli width height]
  (let [posMoved (quot milli (config :milliPerPos))
        [cx cy]  (:pos player)]
    (case dir
      :u (assoc player :pos [ cx  (max 0 (- cy posMoved))])
      :d (assoc player :pos [ cx  (min height (+ cy posMoved))])
      :l (assoc player :pos [ (max 0 (- cx posMoved)) cy])
      :r (assoc player :pos [ (min width (+ cx posMoved)) cy])
    )))

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
          )))
    player))
    
