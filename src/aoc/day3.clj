(ns day3)

(def line-regex #"\#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)$")

(def example-lines ["#1 @ 1,3: 4x4"
                    "#2 @ 3,1: 4x4"
                    "#3 @ 5,5: 2x2"])


(defn line->area [line]
  (let [[_ id l t w h] (re-matches line-regex line)]
    {:left (read-string l)
     :top (read-string t)
     :width (read-string w)
     :height (read-string h)
     :id (read-string id)}))

(def example-claims (map line->area example-lines))


(defn area-coord [area]
  "returns a seq of all x/y covered by this area"
  (let [l (:left area)
        h (:top area)]
    (for [x (range (:width area))
          y (range (:height area))]
      [(+ l x) (+ h y)])))

(defn mark-point
  [img [x y]]
  (update img x #(update % y inc)))

(defn mark-value-point [value img [x y]]
  (update img x #(assoc % y value)))
 

(defn mark-area [map area]
  (let [coords (area-coord area)]
    (reduce mark-point map coords)))

(def empty-map
  (vec (repeat 1000 (vec (repeat  1000 0)))))

(def nil-map
  (vec (repeat 1000 (vec (repeat  1000 nil)))))

(defn part1 [claims]
  (let [map (reduce mark-area empty-map claims)
        flatten-map (reduce concat map)]
    (count (filter (partial <= 2) flatten-map))))

(defn mark-area-id [img area]
  (let [coords (area-coord area)
        id (:id area)]
    (reduce (fn [pimg p] (mark-value-point id pimg p)) img coords)))


(defn- check-overlaps [img claim]
  (let [coords (area-coord claim)
        overlaps (map (partial get-in img) coords)]
    (disj (set overlaps) nil)))

(defn- update-part2 [{:keys [img non-overlap-ids]}  claim]
  "check if the claim overlaps something in the map"
  (let [overlapping-ids (check-overlaps img claim)
        ids-to-remove (if-not (empty?  overlapping-ids)
                        (conj overlapping-ids (:id claim)))]
    {:non-overlap-ids (clojure.set/difference non-overlap-ids ids-to-remove)
     :img (mark-area-id img claim)}))

(defn part2 [claims]
  (let [ initial {:img nil-map
                  :non-overlap-ids (set (map :id claims))}]
    (:non-overlap-ids (reduce update-part2 initial claims))))

(defn -main [arg]
  (with-open [rdr (clojure.java.io/reader arg)]
    (let [lines (line-seq rdr)
          claims (map line->area lines) ]
      (println (part1 claims))
      (println (part2 claims))
      )
    ))

