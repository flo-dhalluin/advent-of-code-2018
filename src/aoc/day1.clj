(ns aoc.day1)

(defn map-with-state [update-fn state coll]
  "create a new coll 
   update-fn : function [state new-val] -> [new-state f]"
  (lazy-seq (when-let [s (seq coll)]
              (let [current (first s)
                    [new-state r] (update-fn state current)]
                (cons r (map-with-state update-fn new-state (rest s)))))))

(defn compute-freq [updates]
  (map-with-state
   (fn [st v]
     (let [new-freq (+ st v)]
       [new-freq new-freq]))
   0 updates))

(defn find-first-dupe [coll]
  (->> coll
       (map-with-state (fn [st v]
                         [(conj st v) [(contains? st v) v]]) #{0})
       (drop-while (comp not first))
       (first)))


(defn -main [arg]
  (with-open [rdr (clojure.java.io/reader arg)]
    (let [updates (->> (line-seq rdr) (map read-string))] 
          (println "Part 1 :" (last (compute-freq updates)))
          (println "Part 2 :" (find-first-dupe (compute-freq (cycle updates)))))))
