(ns tiny-maze.solver)

(def maze1 [[:S 0 1]
            [1  0 1]
            [1  0 :E]])

(defn update-in-maze [poses maze]
  (let [pos (first poses)]
    (if pos
      (recur (rest poses)
             (assoc-in maze [(first pos) (last pos)] :x))
      maze)))

(defn next-pos [x y]
  [
   [(dec x) y]
   [x (dec y)]
   [(inc x) y]
   [x (inc y)]
   ]
  )

(defn maze-val-for-pos [maze x y]
  (try (nth (nth maze x) y)
       (catch Exception e
         :X
         ))
  )

(defn valid-pos [poses maze]
  (filter (fn [pos]
            (let [x (first pos)
                  y (second pos)
                  pos-val (maze-val-for-pos maze x y)]
              (or (= 0 pos-val) (= :S pos-val) (= :E pos-val)))
            )
          poses)
  )

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn rej-pos [poses path]
  (filter (fn [pos]
            (not (in? path pos)) )
          poses))

(defn keep-going [x y path maze]
  (let [next-poses (next-pos x y)
        next-poses (valid-pos next-poses maze)              ;; does not have options => go back
        next-poses (rej-pos next-poses path)
        next-pos (first next-poses)
        next-path (conj path next-pos)]
    (if next-pos
      (recur (first next-pos) (last next-pos) next-path maze)
      (do
        (println (str path))
        path))))

(defn solve-maze [maze]
  (let [walked-path (keep-going 0 0 [[0 0]] maze)]
    (update-in-maze walked-path maze)))
