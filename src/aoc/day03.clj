(ns aoc.day03)

(def input-file-name "input/day03")

(defn file->wires []
  (let [content (slurp input-file-name)
        lines (clojure.string/split-lines content)]
    (->> lines
         (map #(.trim %))
         (map #(clojure.string/split % #",")))))

(def alpha->direction {"L" :left
                       "R" :right
                       "D" :down
                       "U" :up})

(defn parse-move [move]
  (let [match (re-matches #"([LURD])(\d+)" move)
        direction (match 1)
        count (match 2)]
    {:direction (alpha->direction direction)
     :count     (java.lang.Integer/parseInt count)}))

(defn apply-move [[x y] {direction :direction count :count}]
  (case direction
    :left (->> (range 1 (inc count))
               (map #(vector (- x %) y)))
    :up (->> (range 1 (inc count))
             (map #(vector x (+ y %))))
    :right (->> (range 1 (inc count))
                (map #(vector (+ x %) y)))
    :down (->> (range 1 (inc count))
               (map #(vector x (- y %))))))

(defn move-fn [acc move]
  (let [curr-coord (last acc)
        next-coords (apply-move curr-coord move)]
    ;(into [] (concat acc next-coords))))
    (concat acc next-coords)))

(defn wire->coords [wire]
  (let [moves (mapv parse-move wire)]
    (reduce move-fn [[0 0]] moves)))

(def test-wire1
  ["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"])
(def test-wire2
  ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"])

(defn get-distance
  ([[x y]]
   (get-distance [0 0] [x y]))
  ([[px py] [qx qy]]
   (let [dist-x (java.lang.Math/abs ^int (- qx px))
         dist-y (java.lang.Math/abs ^int (- qy py))]
     (+ dist-x dist-y))))

(defn part1 []
  (let [wires (file->wires)
        w1 (->> wires (first) (wire->coords) (set))
        w2 (->> wires (second) (wire->coords) (set))
        intersections (->> (clojure.set/intersection w1 w2)
                           (filter #(not (= [0 0] %))))
        distances (->> intersections
                       (map (fn [a] {:distance (get-distance a)
                                     :coord    a}))
                       (sort-by :distance))]
    (first distances)))

(def part-answer {:distance 557, :coord [181 376]})         ;answer => 557

