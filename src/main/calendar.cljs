(ns calendar
  (:require
   [clojure.string :as str]
   [tick.core :as t]
   [time :refer [days-between days-in-month get-next-weekday
                 get-previous-weekday week-of-month]]))

(def days-of-week [#time/day-of-week "SUNDAY"
                   #time/day-of-week "MONDAY"
                   #time/day-of-week "TUESDAY"
                   #time/day-of-week "WEDNESDAY"
                   #time/day-of-week "THURSDAY"
                   #time/day-of-week "FRIDAY"
                   #time/day-of-week "SATURDAY"])

(defn date->calendar-month
  ([] (date->calendar-month (t/instant)))
  ([date]
   (let [days (days-in-month date)
         start (first days)
         end (last days)
         start-padding (-> (get-previous-weekday start #time/day-of-week "SUNDAY")
                           (days-between start)
                           (butlast))
         end-padding (->> (get-next-weekday end #time/day-of-week "SUNDAY")
                          (days-between end)
                          rest)]
     (concat start-padding days end-padding))))

(defn calendar-year-month-label [date]
  (let [year-month (t/year-month date)]
    [:div {:class "calendar-year-month-label"}
     [:div {:class "calendar-month-label"}
      (-> year-month t/month str str/lower-case str/capitalize)]
     [:div {:class "calendar-year-label"}
      (str (t/year year-month))]]))

(defn calendar-weekday-label [day-of-week]
  (let [day-of-week (-> day-of-week str str/lower-case)]
    [:div {:class (str/join " " ["calendar-weekday-label" day-of-week])}
     (str/capitalize day-of-week)]))


(defn calendar-day [{:keys [cur-date date events title]}]
  (let [calendar-month (t/month cur-date)
        day-of-month (t/day-of-month date)]
    [:div {:class (str/join  " " (cond-> ["calendar-month-day"]
                                   (not= calendar-month (t/month date)) (conj "calendar-padding")
                                   (even? (week-of-month date)) (conj "even-week")))
           :date date
           :dayofweek (t/day-of-week date)}
     [:div {:class "calendar-month-day-of-week"}
      day-of-month]
     [:div {:class "title"} title]
     [:ul {:class "event-list"}
      (for [[i event] (map-indexed vector events)]
        ^{:key i} [:li (:medication event)])]]))

(defn calendar-month
  [{:keys [cur-date events titles] :or {cur-date (t/instant) events [] titles {}}}]
  (let [events-map (group-by :date events)]
    (-> [:div {:class "calendar-month"}
         [calendar-year-month-label cur-date]]
        (into (map (fn [day-of-week] [calendar-weekday-label day-of-week])) days-of-week)
        (into
         (map (fn [date] ^{:key date} [calendar-day {:date date :cur-date cur-date :events (get events-map date) :title (get titles date)}]))
         (date->calendar-month cur-date)))))