(ns aoc.day04)

(def lower-bound 382345)
(def upper-bound 843167)

(defn six-digits? [x]
  (let [x-str (str x)]
    (if (= 6 (.length ^String x-str))
      true
      false)))

(defn in-range?
  ([x]
   (in-range? x lower-bound upper-bound))
  ([x low high]
   (and (>= x low)
        (<= x high))))

(defn adjacent-digits? [x]
  (->> x
       (str)
       (partition-by identity)
       (map count)
       (filter #(>= % 2))
       ((complement empty?))))

(defn increasing? [x]
  (let [x-str (str x)
        x-len (.length ^String x-str)
        increasing? (reduce (fn [a b]
                              (if (> (java.lang.Integer/parseInt ^String (str (get x-str a)))
                                     (java.lang.Integer/parseInt ^String (str (get x-str b))))
                                (reduced false)
                                b))
                            0
                            (range 1 x-len))]
    (if (false? increasing?)
      false
      true)))

(defn part1-password? [x]
  (and (six-digits? x)
       (in-range? x)
       (adjacent-digits? x)
       (increasing? x)))

(defn part1
  ([] (part1 lower-bound upper-bound))
  ([low high]
   (->> (range low (inc high))
        (filter part1-password?)
        (count))))

(def part1-answer 460)

(defn adjacent-digits-strict? [x]
  (->> x
       (str)
       (partition-by identity)
       (map count)
       (filter #(= 2 %))
       ((complement empty?))))

(defn part2-password? [x]
  (and (six-digits? x)
       (in-range? x)
       (adjacent-digits-strict? x)
       (increasing? x)))

(defn part2
  ([] (part2 lower-bound upper-bound))
  ([low high]
   (->> (range low (inc high))
        (filter part2-password?)
        (count))))

(def part2-answer 290)
