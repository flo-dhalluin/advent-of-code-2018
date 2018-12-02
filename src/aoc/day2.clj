(ns aoc.day2)

(defn- inc-nil [v]
  "nil tolerant inc"
  (if v (inc v) 1))

(defn- count-occurences [seq]
  (reduce (fn [st v] (update st v inc-nil)) {} seq))

(defn- count-sets-containing [seq val]
  (reduce (fn [st v]
            (if (contains? v val)
              (inc st) st)) 0 seq))

(defn part1 [lines]
  (let [identics (->> lines
                      (map count-occurences)
                      (map (comp set vals)))]
    (* (count-sets-containing identics 2) (count-sets-containing identics 3))))

(def example1 ["abcdef"
               "bababc"
               "abbcde"
               "abcccd"
               "aabcdd"
               "abcdee"
               "ababab"])

(def example2 ["abcde"
               "fghij"
               "klmno"
               "pqrst"
               "fguij"
               "axcye"
               "wvxyz"])

(defn tree-contains? [ptree s]
  (if-let [i (first s)]
    (if (contains? ptree i)
      (tree-contains? (get ptree i) (rest s))
      false )
    true))

(defn tree-add [ptree s]
  (if-let [i (first s)]
    (update ptree i #(tree-add % (rest s)))))

(defn has-one-diff
  [ptree s]
  (if-let [i (first s)]
    (if (contains? ptree i)
      (has-one-diff (get ptree i) (rest s))
      (when (some #(tree-contains? % (rest s) ) (vals ptree))
        s))))

(defn find-one-diff
  ([lines] 
   (find-one-diff lines {}))

  ([lines tree]
   (if-let [l (first lines)]
     (if-let [sufx (has-one-diff tree l)]
       [l sufx]
       (part2 (rest lines) (tree-add tree l))))))

(defn part2 [lines]
  (if-let [[l suffix] (find-one-diff lines)]
    (str
     (subs l 0 (-  (count l) (count suffix)))
     (rest suffix))))

(defn -main [arg]
  (with-open [rdr (clojure.java.io/reader arg)]
    (let [lines (line-seq rdr)]
      (println (part1 lines))
      (println (part2 lines)))))

