(ns aoc.day02)

(def input-file-name "input/day02")

(defn file->int-codes [path]
  (let [tokens (-> (slurp path)
                   (.trim)
                   (clojure.string/split #","))]
    (mapv #(java.lang.Integer/parseInt %) tokens)))

(defn get-two-operands [current-index int-codes]
  (let [a-index (int-codes (+ 1 current-index))
        b-index (int-codes (+ 2 current-index))
        a (int-codes a-index)
        b (int-codes b-index)]
    [a b]))

(defn add-operands [current-index int-codes]
  (get-two-operands current-index int-codes))

(defn mult-operands [current-index int-codes]
  (get-two-operands current-index int-codes))

(defn add-op [[a b]] (+ a b))

(defn mult-op [[a b]] (* a b))

(def operands-map {1  add-operands
                   2  mult-operands})

(def operations-map {1  add-op
                     2  mult-op})

(defn apply-result [current-index int-codes result]
  (let [result-index (+ 3 current-index)
        result-index-value (int-codes result-index)]
    (assoc int-codes result-index-value result)))

(def part1-stride 4)

(defn restore-int-codes
  [noun verb int-codes]
   (assoc int-codes 1 noun 2 verb))

(defn part1
  ([] (part1 (partial restore-int-codes 12 2)))
  ([restore-fn]
   (let [starting-index 0
         int-codes (file->int-codes input-file-name)
         restored (restore-fn int-codes)]
     (part1 starting-index restored)))
  ([current-index int-codes]
   (let [op-code (int-codes current-index)]
     (if (= op-code 99)
       int-codes
       (let [op-code (int-codes current-index)
             operand-fn (operands-map op-code)
             operands (operand-fn current-index int-codes)
             operation (operations-map op-code)
             op-result (operation operands)
             next-state (apply-result current-index int-codes op-result)
             next-index (+ part1-stride current-index)]
         (recur next-index next-state))))))

(def part1-answer 4714701)

(def part2-goal 19690720)

(defn part2 []
  (let [permutations (for [x (range 100)
                           y (range 100)]
                       [x y])]
    (->> permutations
         (map (fn [[x y]]
                (let [restore-fn (partial restore-int-codes x y)
                      first (first (part1 restore-fn))]
                  [x y first])))
         (filter #(= part2-goal (% 2))))))

(def part2-answer [51 21 19690720])                         ; 100 * 51 + 21 = 5121

