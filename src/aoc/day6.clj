(ns aoc.day6
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]))

(def sample-points
  [[1 1]
   [1 6]
   [8 3]
   [3 4]
   [5 5]
   [8 9]])


(defn expand-point [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])


(defn rect-expand [{:keys [top left bottom right]}
                   [x y]]
  {:top (min top y)
   :left (min left x)
   :right (max right x)
   :bottom (max bottom y)})

(defn compute-bounding-rect [points]
  "compute the bounding rect {:top :left :with :right}"
  (let [[x y] (first points)
        init-rect {:top y :left x :bottom y :right x}]
    (reduce rect-expand init-rect points)))

(def nil-map
  (vec (repeat 1000 (vec (repeat  1000 nil)))))

(defn- rel-coords [r [x y]]
  (let [{:keys [top left]} r
        rx (- x left)
        ry (- y top)]
    [rx ry]))

(defn get-raster-value [r p]
  (get-in (:raster r) (rel-coords r p)))

(defn set-raster-value [r [x y] v]
  (let [[rx ry] (rel-coords r [x y])]
    (update-in r [:raster rx] #(assoc % ry v))))

(defn create-initial-raster [points]
  (let [bbox (compute-bounding-rect points)
        width (inc  (- (:right bbox) (:left bbox)))
        height (inc (- (:bottom bbox) (:top bbox)))
        empty-raster (vec (repeat width (vec (repeat height nil))))
        rast (assoc bbox :raster empty-raster)]
    (println width " x " height)
    (reduce (fn [r [id p]]
              (set-raster-value r p {:id id :dist 0}))
            rast
            (zipmap (range) points))))

(defn- max-nil [a b]
  (if a
    (if b (max a b) a)
    (if b b nil)))

(defn get-max-dist [raster]
  (let [max-by-row #(->> % (map :dist) (reduce max-nil))]
    (reduce max-nil (map max-by-row (:raster raster)))))

(defn upd [v id new-distance]
  (if-not (or (= id ".")
               (= (:id v) id))
    (if-let [dist (:dist v)]
      (cond
        (> dist new-distance) {:id id :dist new-distance}
        (= dist new-distance) {:id "." :dist new-distance}
        :else nil)
      {:id id :dist new-distance})))

(defn in-raster? [rast [x y]]
  (and (<= x (:right rast))
       (>= x (:left rast))
       (<= y (:bottom rast))
       (>= y (:top rast))))

(defn expand-single-point [raster [x y]]
  (let [{:keys [id dist]} (get-raster-value raster [x y])
        next-coords (filter (partial in-raster? raster) (expand-point [x y]))
        new-distance (inc dist)
        updates  (->> next-coords
                      (map (partial get-raster-value raster)) ; get existing entries
                      (map #(upd % id new-distance)) ; compute "updates" 
                      (zipmap next-coords) ; assoc to coords
                      (filterv second) ; keeps non nil update
                      )]
    [(reduce (fn [r [k v]]
               (set-raster-value r k v)) raster updates)
     (keys updates)]))

(defn expand-points [raster pts]
  (reduce (fn [[r next-update] pt]
            (let [[new-raster to-update] (expand-single-point r pt)]
              [new-raster (into next-update to-update)]))
          [raster []]
          pts))

(defn render-raster-stdout [raster]
  (do
    (println "----")
    (doseq [l raster]
      (do
        (doseq [v l]
          (print (get v :id "x")))
        (print "\n")))
    (println "----")))

(defn make-argb [r g b a]
  (int
   (bit-or
    ;;(bit-shift-left a 24)
    (bit-shift-left r 16)
    (bit-shift-left g 8)
    b)))

(def palette
  (into {} 
        (for [i (range 255)]
          (let [r (+ 100 (rand-int 150))
                g (+ 100 (rand-int 150)) 
                b (+ 100 (rand-int 150))]
            [i (make-argb r g b 255)]))))

(defn render-raster-png [raster]
  (let [img-w (count raster)
        img-h (count (first raster))
        img (BufferedImage. img-w img-h BufferedImage/TYPE_3BYTE_BGR)]
    (doseq [[x line] (zipmap (range) raster)]
      (doseq [[y v] (zipmap (range) line)]
        (.setRGB img x y (get palette (:id v) 0))))
    img))

(defn render-raster [raster sfx]
  (let [img (render-raster-png raster)
        fname (File. (str "day6-it-" sfx ".png"))]
    (ImageIO/write img "png" fname)))

(defn voronoize [initial-points]
  (loop [raster (create-initial-raster initial-points)
         it 0
         points-to-look initial-points]
    (let [[new-raster next-points] (expand-points raster points-to-look)]
      (render-raster (:raster new-raster) (format "%05d" it))
      ;;(render-raster-stdout (:raster new-raster))
      (if (empty? next-points)
        new-raster
        (recur new-raster (inc it) next-points)))))

(defn borders-id [raster]
  "all the areas touching a border are -infinite- area"
  (let [top-border (map :id  (first raster))
        bot-border (map :id (peek raster))
        left-border (map (comp :id first) raster )
        right-border (map (comp :id peek) raster)
        all-borders (concat top-border bot-border left-border right-border)]
    (disj (set all-borders) '.')))

(defn max-area [raster]
  (let [areas (frequencies (map :id (flatten raster)))
        inifinite-areas (borders-id raster)
        finite-areas (apply dissoc areas inifinite-areas)]
    (do
      (println inifinite-areas)
      (reduce (fn [[id maxv] [k v]]
                (if (> v maxv)
                  [k v]
                  [id maxv])) finite-areas))))


(defn part1 [points]
  (-> (voronoize points)
      :raster
      max-area))

(defn line->point [li]
  (let [[xs ys] (clojure.string/split li #",")]
    [(read-string xs) (read-string ys)]))

(defn -main [arg]
  (with-open [rdr (clojure.java.io/reader arg)]
    (let [lines (line-seq rdr)
          points (map line->point lines) ]
      (println (part1 points))
;;      (println (part2 claims))
      )
    ))
