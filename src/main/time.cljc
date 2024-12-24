(ns time
  (:require [tick.core :as t]
            [clojure.math :as math]))

(defn ->days [start duration]
  (when start
    (loop [[cur :as dates] (list (t/date start))
           remaining (-> duration t/days dec)]
      (if (>= 0 remaining)
        dates
        (recur (conj dates (t/>> cur #time/period "P1D"))
               (dec remaining))))))

(defn days-between [start-date end-date]
  (loop [cur start-date
         dates []]
    (if (t/>= cur end-date)
      dates
      (recur (t/>> cur #time/period "P1D") (conj dates cur)))))

(defn days-in-month
  ([date] (apply days-in-month ((juxt t/year t/month) (t/date date))))
  ([year month]
   (let [start (t/new-date year month 1)]
     (days-between start (t/>> start #time/period "P1M")))))

(defn get-next-weekday [date weekday]
  (loop [cur date]
    (if (= weekday (t/day-of-week cur))
      cur
      (recur (t/>> cur #time/period "P1D")))))

(defn get-previous-weekday [date weekday]
  (loop [cur date]
    (if (= weekday (t/day-of-week cur))
      cur
      (recur (t/<< cur #time/period "P1D")))))

(defn week-of-month [date]
  (inc (math/floor-div (dec (t/day-of-month date)) 7)))