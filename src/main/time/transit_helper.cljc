(ns time.transit-helper
  "Connect time-literals to transit."
  (:require [cognitect.transit :as transit]
            [time-literals.read-write]
            #?(:cljs [java.time :refer [Period
                                        LocalDate
                                        LocalDateTime
                                        ZonedDateTime
                                        OffsetTime
                                        Instant
                                        OffsetDateTime
                                        ZoneId
                                        DayOfWeek
                                        LocalTime
                                        Month
                                        Duration
                                        Year
                                        YearMonth]]))
  #?(:clj (:import (java.time Period
                              LocalDate
                              LocalDateTime
                              ZonedDateTime
                              OffsetTime
                              Instant
                              OffsetDateTime
                              ZoneId
                              DayOfWeek
                              LocalTime
                              Month
                              Duration
                              Year
                              YearMonth))))

(def time-classes
  {'period Period
   'date LocalDate
   'date-time LocalDateTime
   'zoned-date-time ZonedDateTime
   'offset-time OffsetTime
   'instant Instant
   'offset-date-time OffsetDateTime
   'time LocalTime
   'duration Duration
   'year Year
   'year-month YearMonth
   'zone ZoneId
   'day-of-week DayOfWeek
   'month Month})

(def write-handlers (into {}
                          (for [[tick-class host-class] time-classes]
                            [host-class (transit/write-handler (constantly (name tick-class)) str)])))

(def read-handlers (into {} (for [[sym fun] time-literals.read-write/tags]
                              [(name sym) (transit/read-handler fun)])))