(ns time
  (:require [tick.core :as t]))

(def days-of-week [#time/day-of-week "SUNDAY"
                   #time/day-of-week "MONDAY"
                   #time/day-of-week "TUESDAY"
                   #time/day-of-week "WEDNESDAY"
                   #time/day-of-week "THURSDAY"
                   #time/day-of-week "FRIDAY"
                   #time/day-of-week "SATURDAY"])

(def weekday? (set days-of-week))

(defn ->days [start duration]
  (when start
    (loop [[cur :as dates] (list (t/date start))
           remaining (-> duration t/days dec)]
      (if (>= 0 remaining)
        dates
        (recur (conj dates (t/>> cur #time/period "P1D"))
               (dec remaining))))))

(defn index-of [coll val]
  (first (keep-indexed (fn [i v] (when (= val v) i)) coll)))

(defn period-of-weekdays [start end]
  (when-not (every? weekday? [start end])
    (throw (ex-info (str `period-of-weekdays " only supports #time/day-of-week") {})))
  (t/new-period (mod (apply - (map (partial index-of days-of-week) [end start])) 7) :days))

(defn ->dates [start end]
  (->> (cond
         (every? t/date? [start end])
         [start end]

         (and (t/date? start)
              (t/period? end))
         [start (t/>> start end)]

         (some weekday? [start end])
         (let [period (->> [start end]
                           (map t/day-of-week)
                           (apply period-of-weekdays))]
           [(if (weekday? start) (t/<< end period) start)
            (if (weekday? end) (t/>> start period) end)])

         :else
         (throw (ex-info (str `->dates " doesn't support input arguments") {:start start :end end})))
       (map t/date)))

(defn date-seq [start period]
  (iterate #(t/>> % period) start))

(defn date-range [start end period]
  (take-while #(t/< % end) (date-seq start period)))