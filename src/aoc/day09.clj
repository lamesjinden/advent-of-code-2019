(ns aoc.day09
  (:require [clojure.core.async :as async]))

(def input-file-name "input/day09")

(defn str->int-codes [str]
  (let [tokens (-> str
                   (.trim)
                   (clojure.string/split #","))]
    (->> tokens
         (map #(Long/parseLong %))
         (map-indexed #(vector %1 %2))
         (into {}))))

(defn file->int-codes [path]
  (str->int-codes (slurp path)))

;; ---=== Operands ===---

(defn get-operand [{:keys [int-codes value relative-base mode]}]
  (case mode
    0 (get int-codes value 0)
    1 value
    2 (let [address (+ relative-base value)
            result (get int-codes address 0)]
        result)))

(defn get-out-operand [{:keys [int-codes value relative-base mode]}]
  (case mode
    0 (get int-codes value 0)
    2 (+ relative-base (get int-codes value 0))))

(defn nth-operand [{:keys [current-index int-codes relative-base mode]} n-offset]
  (let [value (get int-codes (+ n-offset current-index) 0)]
    (get-operand {:int-codes     int-codes
                  :value         value
                  :relative-base relative-base
                  :mode          mode})))

(defn first-operand [{:keys [mode1] :as context}]
  (nth-operand (assoc context :mode mode1) 1))

(defn second-operand [{:keys [mode2] :as context}]
  (nth-operand (assoc context :mode mode2) 2))

(defn get-two-operands [context]
  [(first-operand context)
   (second-operand context)])

(defn get-add-operands [context]
  [(first-operand context)
   (second-operand context)
   (get-out-operand (-> context
                        (assoc :value (+ 3 (:current-index context)))
                        (assoc :mode (:mode3 context))))])

(defn get-mult-operands [context]
  [(first-operand context)
   (second-operand context)
   (get-out-operand (-> context
                        (assoc :value (+ 3 (:current-index context)))
                        (assoc :mode (:mode3 context))))])

(defn get-store-operand [context]
  (get-out-operand (-> context
                       (assoc :value (+ 1 (:current-index context)))
                       (assoc :mode (:mode1 context)))))

(defn get-output-operand [context]
  (first-operand context))

(defn get-jump-if-true-operands [context]
  (get-two-operands context))

(defn get-jump-if-false-operands [context]
  (get-two-operands context))

(defn get-less-than-operands [context]
  [(first-operand context)
   (second-operand context)
   (get-out-operand (-> context
                        (assoc :value (+ 3 (:current-index context)))
                        (assoc :mode (:mode3 context))))])

(defn get-equals-operands [context]
  [(first-operand context)
   (second-operand context)
   (get-out-operand (-> context
                        (assoc :value (+ 3 (:current-index context)))
                        (assoc :mode (:mode3 context))))])

(defn get-adjust-base-operand [context]
  (first-operand context))

(def operands-map {1 get-add-operands
                   2 get-mult-operands
                   3 get-store-operand
                   4 get-output-operand
                   5 get-jump-if-true-operands
                   6 get-jump-if-false-operands
                   7 get-less-than-operands
                   8 get-equals-operands
                   9 get-adjust-base-operand})

;; ---=== Operations ===---

(defn add-op [{[x y] :operands-result}]
  (+ x y))

(defn mult-op [{[x y] :operands-result}]
  (* x y))

(defn store-op [input & _]
  (async/<!! input))

(defn output-op [output {x :operands-result}]
  (async/>!! output x))

(defn jump-if-true-op [{[x y] :operands-result}]
  (if (zero? x)
    nil
    y))

(defn jump-if-false-op [{[x y] :operands-result}]
  (if (zero? x)
    y
    nil))

(defn less-than-op [{[x y] :operands-result}]
  (if (< x y)
    1
    0))

(defn equals-op [{[x y] :operands-result}]
  (if (= x y)
    1
    0))

(defn adjust-base-op [{x :operands-result}] x)

(def operations-map {1 add-op
                     2 mult-op
                     3 store-op
                     4 output-op
                     5 jump-if-true-op
                     6 jump-if-false-op
                     7 less-than-op
                     8 equals-op
                     9 adjust-base-op})

;; ---=== Update ===---

(defn apply-two-arg-result [{:keys [int-codes operations-result], [_ _ result-address] :operands-result, :as context}]
  (let [apply-result (assoc int-codes result-address operations-result)]
    (assoc context :int-codes apply-result)))

(defn apply-one-arg-result [{:keys [int-codes operations-result operands-result] :as context}]
  (let [apply-result (assoc int-codes operands-result operations-result)]
    (assoc context :int-codes apply-result)))

(def apply-noop identity)

(defn apply-adjust-base [{:keys [operations-result relative-base] :as context}]
  (let [apply-result (+ relative-base operations-result)]
    (assoc context :relative-base apply-result)))

(def updates-map {1 apply-two-arg-result
                  2 apply-two-arg-result
                  3 apply-one-arg-result
                  4 apply-noop
                  5 apply-noop
                  6 apply-noop
                  7 apply-two-arg-result
                  8 apply-two-arg-result
                  9 apply-adjust-base})

;; ---=== Stride ===----

(defn get-next-stride [stride {:keys [current-index] :as context}]
  (let [next-index (+ stride current-index)]
    (assoc context :next-index next-index)))

(defn get-next-stride-jump [{:keys [current-index operations-result] :as context}]
  (let [next-index (if (nil? operations-result)
                     (+ current-index 3)
                     operations-result)]
    (assoc context :next-index next-index)))

(def strides-map {1 (partial get-next-stride 4)
                  2 (partial get-next-stride 4)
                  3 (partial get-next-stride 2)
                  4 (partial get-next-stride 2)
                  5 get-next-stride-jump
                  6 get-next-stride-jump
                  7 (partial get-next-stride 4)
                  8 (partial get-next-stride 4)
                  9 (partial get-next-stride 2)})

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

(defn build-context
  ([]
   (build-context (file->int-codes input-file-name)))
  ([int-codes]
   {:int-codes     int-codes
    :current-index 0
    :relative-base 0}))

(defn run-computer
  ([computer]
   (let [context (build-context)]
     (run-computer computer context)))
  ([{:keys [operands operations updates strides] :as computer}
    {:keys [current-index int-codes] :as context}]
   (let [op-code (int-codes current-index)]
     (if (= op-code 99)
       (println "---==== HALT ===---")
       (let [{:keys [op mode1 mode2 mode3]} (decode-op-code op-code)
             _ (when (not (contains? (set (keys (:operands computer))) op))
                 (throw (Exception. (str "invalid op code:" op))))

             operands-fn (operands op)
             operands-params (-> context
                                 (assoc :mode1 mode1)
                                 (assoc :mode2 mode2)
                                 (assoc :mode3 mode3))
             operands-result (operands-fn operands-params)

             operations-fn (operations op)
             operations-params (assoc operands-params :operands-result operands-result)
             operations-result (operations-fn operations-params)

             updates-fn (updates op)
             updates-params (assoc operations-params :operations-result operations-result)
             updates-result (updates-fn updates-params)

             strides-fn (strides op)
             strides-params (assoc updates-params :updates-result updates-result)
             strides-result (strides-fn strides-params)]
         (recur
           computer
           (-> context
               (assoc :current-index (:next-index strides-result))
               (assoc :int-codes (:int-codes updates-result))
               (assoc :relative-base (:relative-base updates-result)))))))))

(def part1-test1 "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
(def part1-test2 "1102,34915192,34915192,7,4,7,99,0")
(def part1-test3 "104,1125899906842624,99")

(defn run-program [input-instruction]
  (let [input (async/chan 1)
        output (async/chan 1)
        computer (build-computer input output)
        context (build-context (file->int-codes input-file-name))]
    (async/>!! input input-instruction)
    (run-computer computer context)
    (async/go-loop [x (async/<!! output)]
      (when (not (nil? x))
        (println "->" x)
        (recur (async/<!! output))))))

(defn part1 []
  (run-program 1))

(def part1-answer 3100786347)

(defn part2 []
  (run-program 2))

(def part2-answer 87023)
