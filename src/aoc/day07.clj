(ns aoc.day07
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.async :as async]))

(def input-file-name "input/day07")

(defn str->int-codes [str]
  (let [tokens (-> str
                   (.trim)
                   (clojure.string/split #","))]
    (mapv #(Integer/parseInt %) tokens)))

(defn file->int-codes [path]
  (str->int-codes (slurp path)))

(defn get-operand [int-codes value mode]
  (if (zero? mode)
    (get int-codes value)
    value))

(defn nth-operand [current-index int-codes mode offset]
  (let [value (int-codes (+ offset current-index))]
    (get-operand int-codes value mode)))

(defn first-operand [current-index int-codes mode]
  (nth-operand current-index int-codes mode 1))

(defn second-operand [current-index int-codes mode]
  (nth-operand current-index int-codes mode 2))

(defn third-operand [current-index int-codes mode]
  (nth-operand current-index int-codes mode 3))

(defn get-two-operands [current-index int-codes mode1 mode2]
  [(first-operand current-index int-codes mode1)
   (second-operand current-index int-codes mode2)])

(defn add-operands [current-index int-codes mode1 mode2 & _]
  (get-two-operands current-index int-codes mode1 mode2))

(defn mult-operands [current-index int-codes mode1 mode2 & _]
  (get-two-operands current-index int-codes mode1 mode2))

(defn store-operands [current-index int-codes mode & _]
  (first-operand current-index int-codes mode))

(defn output-operands [current-index int-codes mode & _]
  (first-operand current-index int-codes mode))

(defn jump-if-true-operands [current-index int-codes mode1 mode2 & _]
  (get-two-operands current-index int-codes mode1 mode2))

(defn jump-if-false-operands [current-index int-codes mode1 mode2 & _]
  (get-two-operands current-index int-codes mode1 mode2))

(defn less-than-operands [current-index int-codes mode1 mode2 & _]
  (get-two-operands current-index int-codes mode1 mode2))

(defn equals-operands [current-index int-codes mode1 mode2 & _]
  (get-two-operands current-index int-codes mode1 mode2))

(def operands-map {1 add-operands
                   2 mult-operands
                   3 store-operands
                   4 output-operands
                   5 jump-if-true-operands
                   6 jump-if-false-operands
                   7 less-than-operands
                   8 equals-operands})

(defn add-op [[a b]]
  (+ a b))

(defn mult-op [[a b]]
  (* a b))

(defn store-op [input & _]
  (async/<!! input))

(defn output-op [output x & _]
  (async/>!! output x))

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
  (let [result-address (third-operand current-index int-codes mode)]
    (assoc int-codes result-address op-result)))

(defn apply-one-arg-result [current-index int-codes op-result mode]
  (let [result-address (first-operand current-index int-codes mode)]
    (assoc int-codes result-address op-result)))

(defn apply-noop [_ int-codes _ _] int-codes)

(def updates-map {1 apply-two-arg-result
                  2 apply-two-arg-result
                  3 apply-one-arg-result
                  4 apply-noop
                  5 apply-noop
                  6 apply-noop
                  7 apply-two-arg-result
                  8 apply-two-arg-result})

(defn get-next-stride [stride current-index & _]
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
               (Integer/parseInt))
        mode1 (-> padded
                  (get 2)
                  (str)
                  (Integer/parseInt))
        mode2 (-> padded
                  (get 1)
                  (str)
                  (Integer/parseInt))
        mode3 (-> padded
                  (get 0)
                  (str)
                  (Integer/parseInt))]
    {:op    op
     :mode1 mode1
     :mode2 mode2
     :mode3 mode3}))

(defn build-computer
  ([input output]
   {:operands   operands-map
    :operations (-> operations-map
                    (update-in [3] (fn [x] (partial x input)))
                    (update-in [4] (fn [x] (partial x output))))
    :updates    updates-map
    :strides    strides-map}))

(def test-program1-part2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
(def test-program1-answer 139629729)

(def test-program2-part2 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
(def test-program2-answer 18216)

(defn run-computer
  ([computer] (run-computer computer (file->int-codes input-file-name)))
  ([computer int-codes] (run-computer computer 0 int-codes))
  ([{:keys [operands operations updates strides] :as computer} current-index int-codes]
   (let [op-code (int-codes current-index)]
     (if (= op-code 99)
       {:current-index current-index
        :int-codes     int-codes
        }
       ;todo - when is mode 3 used?
       (let [{:keys [op mode1 mode2]} (decode-op-code op-code)
             operands-fn (operands op)
             operands-result (operands-fn current-index int-codes mode1 mode2)
             operations-fn (operations op)
             operations-result (operations-fn operands-result)
             updates-fn (updates op)
             updates-result (updates-fn current-index int-codes operations-result 1)
             strides-fn (strides op)
             strides-result (strides-fn current-index operations-result)]
         (recur computer strides-result updates-result))))))

(defn run-program-part1 [settings]
  (let [channel (async/chan 2)
        computer (build-computer channel channel)]
    (reduce (fn [signal setting]
              (async/>!! channel setting)
              (async/>!! channel signal)
              (run-computer computer)
              (async/<!! channel))
            0
            settings)))

(def phase-settings-part1 [0 1 2 3 4])

(defn part1 []
  (let [combinations (combo/permutations phase-settings-part1)]
    (->> combinations
         (map (fn [combo] (run-program-part1 combo)))
         (apply max))))

(def part1-answer 368584)

(def phase-settings-part2 [5 6 7 8 9])

(defn run-program-part2 [settings]
  (let [a->b (async/chan 2)
        b->c (async/chan 2)
        c->d (async/chan 2)
        d->e (async/chan 2)
        e->a (async/chan 2)

        ampA (build-computer e->a a->b)
        ampB (build-computer a->b b->c)
        ampC (build-computer b->c c->d)
        ampD (build-computer c->d d->e)
        ampE (build-computer d->e e->a)]

    ; init settings values
    (async/>!! e->a (get settings 0))
    (async/>!! a->b (get settings 1))
    (async/>!! b->c (get settings 2))
    (async/>!! c->d (get settings 3))
    (async/>!! d->e (get settings 4))

    ; init ampA seed value
    (async/>!! e->a 0)

    (async/go (run-computer ampA))
    (async/go (run-computer ampB))
    (async/go (run-computer ampC))
    (async/go (run-computer ampD))
    (let [complete (async/go (run-computer ampE))           ; use ampE to complete
          _ (async/<!! complete)                            ; await ameE result
          result (async/<!! e->a)]                          ; get ampE result

      ; clean-up
      (async/close! a->b)
      (async/close! b->c)
      (async/close! c->d)
      (async/close! d->e)
      (async/close! e->a)

      ; return result
      result)))

(defn part2 []
  (let [combinations (combo/permutations phase-settings-part2)]
    (->> combinations
         (map (fn [combo] (run-program-part2 combo)))
         (apply max))))

(def part2-answer 35993240)
