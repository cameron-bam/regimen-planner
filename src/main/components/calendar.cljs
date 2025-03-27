(ns components.calendar
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [tick.core :as t]
   [time :refer [->dates date-range days-of-week]]))

(defn get-calendar-days
  ([] (get-calendar-days {:year-month (t/year-month)}))
  ([{:keys [date-data year-month]}]
   (let [start (t/new-date (t/year year-month) (t/month year-month) 1)]
     (date-range (first (->dates #time/day-of-week "SUNDAY" (apply t/min start (keys date-data))))
                 (second (->dates (apply t/max (t/>> start #time/period "P1M") (keys date-data)) #time/day-of-week "SUNDAY"))
                 #time/period "P1D"))))

(defn calendar-year-month-label [year-month]
  [:div {:class "calendar-year-month-label"}
   [:div {:class "calendar-month-label"}
    (-> year-month t/month str str/lower-case str/capitalize)]
   [:div {:class "calendar-year-label"}
    (str (t/year year-month))]])

(defn calendar-weekday-label [day-of-week]
  (let [day-of-week (-> day-of-week str str/lower-case)]
    [:div {:class (str/join " " ["calendar-weekday-label" day-of-week])}
     (str/capitalize day-of-week)]))


(defn calendar-day [{:keys [year-month date multiple-months? week date-data date-renderer]}]
  (let [calendar-month (t/month year-month)
        day-of-month (t/day-of-month date)]
    [:div {:class (str/join  " " (cond-> ["calendar-month-day"]
                                   (not= calendar-month (t/month date)) (conj "calendar-padding")
                                   (odd? week) (conj "even-week")))
           :date date
           :dayofweek (t/day-of-week date)}
     (when (or multiple-months?
               (= year-month (t/year-month date)))
       [:div {:class "calendar-month-day-of-week"}
        day-of-month])
     (when (and date-data date-renderer)
       [date-renderer date-data])]))

(defn pick-year-month [date-data]
  (if-not (seq date-data)
    nil
    (->> date-data
         keys
         set
         (map t/year-month)
         (group-by identity)
         (map #(update % 1 count))
         (sort-by (comp (partial * -1) second))
         ffirst)))

(defn calendar-month
  [{:keys [date-data date-renderer year-month] :or {date-data {}}}]
  (let [year-month (or year-month (pick-year-month date-data) (t/year-month))
        multiple-months? (some #(not= year-month (t/year-month %)) (keys date-data))]
    (-> [:div {:class "calendar-month page-break-after"}
         [calendar-year-month-label year-month]]
        (into (map (fn [day-of-week] [calendar-weekday-label day-of-week])) days-of-week)
        (into
         (comp
          (map-indexed vector)
          (map (fn [[i date]] ^{:key date} [calendar-day {:date date
                                                          :week (math/floor-div i 7)
                                                          :year-month year-month
                                                          :date-data (get date-data date)
                                                          :multiple-months? multiple-months?
                                                          :date-renderer date-renderer}])))
         (get-calendar-days {:year-month year-month :date-data date-data})))))