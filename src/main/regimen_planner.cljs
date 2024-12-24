(ns regimen-planner
  (:require
   [reagent.dom :as rdom]
   [regimen :as regimen]
   [tick.core :as t]
   [calendar :refer [calendar-month]]
   [clojure.pprint :refer [pprint]]))

(defn main []
  [:div {:style {:padding "24px 24px 24px 24px"}}
   (let [input {:treatment-date (t/date "2024-12-12")
                :regimen :tchp-chemo}]
     [calendar-month {:cur-date (:treatment-date input)
                      :titles {(t/date (:treatment-date input)) "Treatment Date"}
                      :events (doto (regimen/get-regimen-events input) pprint)}])])

(defn init []
  (rdom/render [main] (js/document.getElementById "root")))