(ns aoc.day01)

(defn get-fuel-required [module-mass]
  (-> module-mass
      (/ 3)
      (Math/floor)
      (- 2)
      (int)))

(def input-file-name "input/day01")

(defn part1 []
  (let [input (slurp input-file-name)
        lines (clojure.string/split-lines input)
        masses (map #(Integer/parseInt %) lines)
        req-fuels (map get-fuel-required masses)]
    (reduce + 0 req-fuels)))

(def part1-answer 3506577)

(defn get-fuel-required-rec [module-mass]
  (loop [fuel-required (get-fuel-required module-mass)]
    (if (< fuel-required 0)
      (lazy-seq (cons 0 (get-fuel-required-rec 0)))
      (lazy-seq (cons fuel-required (get-fuel-required-rec fuel-required))))))

(defn part2 []
  (let [input (slurp input-file-name)
        lines (clojure.string/split-lines input)
        masses (map #(Integer/parseInt %) lines)
        req-fuels (->> masses
                       (map (fn [x]
                              (->> x
                                   (get-fuel-required-rec)
                                   (take-while pos?)
                                   (reduce +)))))]
    (reduce + 0 req-fuels)))

(def part2-answer 5256960)