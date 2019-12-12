(ns aoc.day08
  (:require [clojure.string :as str]))

(def input-file-name "input/day08")

(def part1-pixel-width 25)
(def part1-pixel-height 6)

(defn str->ints [s]
  (->> s
       (seq)
       (map #(java.lang.Integer/parseInt (str %)))))

(defn layer->pixels
  ([layer]
   (layer->pixels part1-pixel-width layer))
  ([pixel-width layer]
   (->> layer
        (str->ints)
        (partition pixel-width)
        (map vec)
        (into []))))

(defn str->layers
  ([str] (str->layers part1-pixel-width part1-pixel-height str))
  ([pixel-width pixel-height str]
   (->> str
        (partition (* pixel-width pixel-height))
        (map (fn [x] (layer->pixels pixel-width x)))
        (into []))))

(defn str->pixels
  ([str]
   (str->pixels part1-pixel-width part1-pixel-height str))
  ([pixel-width pixel-height str]
   (->> str
        (str->layers pixel-width pixel-height)
        (into []))))

(defn file->pixels [path]
  (-> (slurp path)
      (str/trim)
      (str->pixels)))

(defn count-instances [n coll]
  (->> coll
       (flatten)
       (filter #(= % n))
       (count)))

(defn part1 []
  (let [layers (file->pixels input-file-name)
        fewest-zeros (->> layers
                          (sort-by #(count-instances 0 %))
                          (first))
        ]
    (* (count-instances 1 fewest-zeros) (count-instances 2 fewest-zeros))))

(def part1-answer 2193)

(defn merge-colors [front back]
  (if (or (= 0 front) (= 1 front))
    front
    back))

(defn get-2d-value [row col grid]
  (get (get grid row) col))

(defn fill! [val row col grid]
  (dorun (for [x (range row) y (range col)]
           (aset grid x y val)))
  grid)

(defn merge-layers [layers]
  (let [rows (count (first layers))
        cols (count (first (first layers)))
        merged (fill! 2 rows cols (make-array Long/TYPE rows cols))]
    (dorun (for [row (range rows)
                 col (range cols)]
             (let [values (mapv #(get-2d-value row col %) layers)
                   color (reduce merge-colors values)]
               (aset merged row col color))))
    (mapv #(into [] %) merged)))

(defn part2 []
  (let [layers (file->pixels input-file-name)
        merged (merge-layers layers)]
    merged))

(def part2-answer "yehef")