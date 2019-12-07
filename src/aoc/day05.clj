(ns aoc.day05)

(def input-file-name "input/day05")

(defn str->int-codes [str]
  (let [tokens (-> str
                   (.trim)
                   (clojure.string/split #","))]
    (mapv #(java.lang.Integer/parseInt %) tokens)))

(defn file->int-codes [path]
  (str->int-codes (slurp path)))

(defn get-operand [int-codes value mode]
  (if (zero? mode)
    (get int-codes value)
    value))

(defn get-x-operand [current-index int-codes mode offset]
  (let [value (int-codes (+ offset current-index))]
    (get-operand int-codes value mode)))

(defn get-first-operand [current-index int-codes mode]
  (get-x-operand current-index int-codes mode 1))

(defn get-second-operand [current-index int-codes mode]
  (get-x-operand current-index int-codes mode 2))

(defn get-third-operand [current-index int-codes mode]
  (get-x-operand current-index int-codes mode 3))

(defn get-two-operands [current-index int-codes mode1 mode2]
  [(get-first-operand current-index int-codes mode1)
   (get-second-operand current-index int-codes mode2)])

(defn add-operands [current-index int-codes mode1 mode2 & rest]
  (get-two-operands current-index int-codes mode1 mode2))

(defn mult-operands [current-index int-codes mode1 mode2 & rest]
  (get-two-operands current-index int-codes mode1 mode2))

(defn store-operands [current-index int-codes mode & rest]
  (get-first-operand current-index int-codes mode))

(defn output-operands [current-index int-codes mode & rest]
  (get-first-operand current-index int-codes mode))

(defn jump-if-true-operands [current-index int-codes mode1 mode2 & rest]
  (get-two-operands current-index int-codes mode1 mode2))

(defn jump-if-false-operands [current-index int-codes mode1 mode2 & rest]
  (get-two-operands current-index int-codes mode1 mode2))

(defn less-than-operands [current-index int-codes mode1 mode2 & rest]
  (get-two-operands current-index int-codes mode1 mode2))

(defn equals-operands [current-index int-codes mode1 mode2 & rest]
  (get-two-operands current-index int-codes mode1 mode2))

(def operands-map {1 add-operands
                   2 mult-operands
                   3 store-operands
                   4 output-operands
                   5 jump-if-true-operands
                   6 jump-if-false-operands
                   7 less-than-operands
                   8 equals-operands})

(defn add-op [[a b]] (+ a b))

(defn mult-op [[a b]]
  (* a b))

(defn store-op [_]
  (println "Enter Test ID:")
  (-> (read-line)
      (.trim)
      (java.lang.Integer/parseInt)))

(defn output-op [x] (println x))

(defn jump-if-true-op [[x y]]
  (if (zero? x)
    0
    y))

(defn jump-if-false-op [[x y]]
  (if (zero? x)
    y
    0))

(defn less-than-op [[x y]]
  (if (< x y)
    1
    0))

(defn equals-op [[x y]]
  (if (= x y)
    1
    0))

(def operations-map {1 add-op
                     2 mult-op
                     3 store-op
                     4 output-op
                     5 jump-if-true-op
                     6 jump-if-false-op
                     7 less-than-op
                     8 equals-op})

(defn apply-two-arg-result [current-index int-codes op-result mode]
  (let [result-address (get-third-operand current-index int-codes mode)]
    (assoc int-codes result-address op-result)))

(defn apply-one-arg-result [current-index int-codes op-result mode]
  (let [result-address (get-first-operand current-index int-codes mode)]
    (assoc int-codes result-address op-result)))

(defn apply-noop [current-index int-codes op-result mode] int-codes)

(def update-map {1 apply-two-arg-result
                 2 apply-two-arg-result
                 3 apply-one-arg-result
                 4 apply-noop
                 5 apply-noop
                 6 apply-noop
                 7 apply-two-arg-result
                 8 apply-two-arg-result})

(defn get-next-stride [stride current-index & rest]
  (+ stride current-index))

(defn get-next-stride-jump [current-index op-result]
  (if (zero? op-result)
    (+ current-index 3)
    op-result))

(def strides-map {1 (partial get-next-stride 4)
                  2 (partial get-next-stride 4)
                  3 (partial get-next-stride 2)
                  4 (partial get-next-stride 2)
                  5 get-next-stride-jump
                  6 get-next-stride-jump
                  7 (partial get-next-stride 4)
                  8 (partial get-next-stride 4)})

(defn decode-op-code [op-code]
  (let [padded (format "%05d" op-code)
        op (-> padded
               (.substring 3)
               (java.lang.Integer/parseInt))
        mode1 (-> padded
                  (get 2)
                  (str)
                  (java.lang.Integer/parseInt))
        mode2 (-> padded
                  (get 1)
                  (str)
                  (java.lang.Integer/parseInt))
        mode3 (-> padded
                  (get 0)
                  (str)
                  (java.lang.Integer/parseInt))]
    {:op    op
     :mode1 mode1
     :mode2 mode2
     :mode3 mode3}))

(defn part1
  ([] (part1 0 (file->int-codes input-file-name)))
  ([int-codes] (part1 0 (str->int-codes int-codes)))
  ([current-index int-codes]
   (let [op-code (int-codes current-index)]
     (if (= op-code 99)
       int-codes
       (let [{:keys [op mode1 mode2 mode3]} (decode-op-code op-code)
             operands-fn (operands-map op)
             operands-result (operands-fn current-index int-codes mode1 mode2)
             operations-fn (operations-map op)
             operations-result (operations-fn operands-result)
             updates-fn (update-map op)
             updates-result (updates-fn current-index int-codes operations-result 1)
             strides-fn (strides-map op)
             strides-result (strides-fn current-index operations-result)]
         (recur strides-result updates-result))))))

(def part1-answer 2845163)

(defn part2 []
  (part1))

(comment
  (part1 "1002,4,3,4,33")
  (part1 "3,0,4,0,99")
  (part1 "3,9,8,9,10,9,4,9,99,-1,8")
  (part1 "3,9,7,9,10,9,4,9,99,-1,8")
  (part1 "3,3,1108,-1,8,3,4,3,99")
  (part1 "3,3,1107,-1,8,3,4,3,99")
  (part1 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
  (part1 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
  (part1 "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
  )

(def part2-answer 9436229)