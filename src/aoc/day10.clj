(ns aoc.day10)

(def input-file-name "input/day10")

(def input-example1 ".#..#\n.....\n#####\n....#\n...##")
(def input-example2 ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
(def input-example3 ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##")

(defn string->grid [s]
  (->> s
       (clojure.string/split-lines)
       (mapv vec)))

(defn get-coord [grid x-col y-row]
  (get-in grid [y-row x-col]))

(defn string->asteroids [s]
  (let [grid (string->grid s)
        rows (count grid)
        cols (count (first grid))]
    (->> (for [x (range cols) y (range rows)] [x y])
         (filter #(= \# (get-coord grid (first %) (second %))))
         (into #{}))))

(defn file->asteroids [path]
  (-> path
      (slurp)
      (string->asteroids)))

(defn get-distance
  [[x1 y1] [x2 y2]]
  (let [deltax (- x2 x1)
        deltay (- y2 y1)
        deltax-squared (Math/pow deltax 2)
        deltay-squared (Math/pow deltay 2)
        squared-sum (+ deltax-squared deltay-squared)
        sqrt-sum (Math/sqrt squared-sum)]
    sqrt-sum))

(defn get-bearing [[x1 y1] [x2 y2]]
  (let [deltax (- x2 x1)
        deltay (- y2 y1)
        bearing (mod (+ 360 (* (/ 180 Math/PI) (Math/atan2 deltay deltax))) 360)]
    bearing))

(defn get-observables [asteroids asteroid]
  (->> asteroids
       (filter #(not= % asteroid))
       (map (fn [i]
              (let [distance (get-distance asteroid i)
                    bearing (get-bearing asteroid i)]
                {:asteroid i
                 :bearing  bearing
                 :distance distance})))
       (group-by :bearing)
       (vals)
       (map #(apply min-key :distance %))))

(defn max-observables [asteroids]
  (->> asteroids
       (map (fn [x] {:asteroid    x
                     :observables (get-observables asteroids x)}))
       (map (fn [x] (assoc x :observables-count (count (:observables x)))))
       (apply max-key :observables-count)))

(defn part1
  ([]
   (part1 (file->asteroids input-file-name)))
  ([asteroids]
   (-> asteroids
       (max-observables)
       (:observables-count))))

(defn get-targets [asteroids asteroid-coords]
  (->> asteroids
       (filter #(not= % asteroid-coords))
       (map (fn [i]
              (let [distance (get-distance asteroid-coords i)
                    bearing (get-bearing asteroid-coords i)]
                {:asteroid i
                 :bearing  bearing
                 :distance distance})))
       (sort-by :distance)
       (group-by :bearing) ; group-by maps to a vector. coo.
       (into (sorted-map))))

(def starting-bearing 270) ; because up is down

(defn get-victims
  ([targets]
   (let [[front back] (split-with #(< % starting-bearing) (keys targets))
         oriented-up-keys (cycle (concat back front))
         targets-count (->> targets (vals) (map count) (reduce +))]
     (get-victims oriented-up-keys targets targets-count)))
  ([target-bearings targets remaining-targets]
   (let [target-bearing (first target-bearings)
         victim (first (get targets target-bearing))]
     (when (not (zero? remaining-targets))
       (lazy-seq
         (cons
           victim
           (get-victims
             (rest target-bearings)
             (update-in targets [target-bearing] rest)
             (if (not (nil? victim)) (dec remaining-targets) remaining-targets))))))))

(defn part2
  ([]
   (part2 (file->asteroids input-file-name)))
  ([asteroids]
   (let [asteroid (max-observables asteroids)
         asteroid-coords (:asteroid asteroid)
         targets (get-targets asteroids asteroid-coords)
         victims (->> targets (get-victims) (filter some?))
         victim (nth victims 199)
         {[x y] :asteroid} victim]
     (+ y (* 100 x)))))