(ns aoc.day06)

(def input-file-name "input/day06")

(defn string->orbit [str]
  (let [matches (re-matches #"(\w+)\)(\w+)" str)
        orbited (get matches 1)
        orbiting (get matches 2)]
    {:orbited  orbited
     :orbiting orbiting}))

(defn strings->orbits [strings]
  (map string->orbit strings))

(defn file->orbits [path]
  (-> path
      (slurp)
      (clojure.string/split-lines)
      (strings->orbits)))

(defn reduce-orbit [graph {:keys [orbited orbiting]}]
  (let [insert-orbited (fn [g] (if (not (contains? graph orbited))
                                 (assoc g orbited nil)
                                 g))]
    (-> graph
        insert-orbited
        (assoc orbiting orbited))))

(defn orbits->graph [orbits]
  (reduce reduce-orbit {} orbits))

(defn orbit-path
  ([graph object]
   (orbit-path graph object []))
  ([graph object path]
   (let [orbited (get graph object)]
     (if (nil? orbited)
       path
       (recur graph orbited (conj path orbited))))))

(defn orbit-distance [graph object]
  (count (orbit-path graph object)))

(defn total-distances [graph]
  (->> graph
       (keys)
       (map #(orbit-distance graph %))
       (reduce + 0)))

(defn part1
  ([]
   (part1 (-> input-file-name
              (file->orbits)
              (orbits->graph))))
  ([graph]
   (total-distances graph)))

(def part1-answer 402879)

(defn first-common-element [seq1 seq2]
  (let [vec1 (vec seq1)
        vec2 (vec seq2)
        ordered (if (> (count vec1) (count vec2))
                  [vec1 vec2]
                  [vec2 vec1])
        [outer inner] ordered
        common (filter (fn [x]
                         (if (some #(= x %) inner)
                           true
                           false)) outer)]
    (first common)))

(defn orbital-transfers [graph you san]
  (let [you-path (orbit-path graph you)
        san-path (orbit-path graph san)
        common (first-common-element you-path san-path)
        you-path-to-common (take-while #(not (= % common)) you-path)
        san-path-to-common (take-while #(not (= % common)) san-path)]
    (+ (count you-path-to-common) (count san-path-to-common))))

(def p2-sample ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"])

(defn part2
  ([]
   (let [graph (-> input-file-name
                   (file->orbits)
                   (orbits->graph))
         you "YOU"
         san "SAN"]
     (part2 graph you san)))
  ([graph you san]
   (orbital-transfers graph you san)))

(def p2-answer "484")
