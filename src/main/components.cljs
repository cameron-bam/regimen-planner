(ns components
  (:require
   ["@heroicons/react/16/solid" :refer [PlusIcon EqualsIcon]]
   ["react-sortablejs" :refer [ReactSortable]]
   [reagent.core :as r]))

(defn plus-selector [& options]
  (r/with-let [open? (r/atom false)]
    [:div {:class "flex relative pt-2"}
     [:button {:style {:height "20px" :width "20px" :overflow "hidden"}
               :on-click #(reset! open? true)
               :on-blur #(reset! open? false)
               :class "border-solid border-black border rounded-full content-center flex mx-auto hover:bg-gray-300 bg-transparent"}
      [:> PlusIcon {:class "h-full content-center"}]]
     [:div {:style {:visibility (if @open? "visible" "hidden")
                    :position "absolute"
                    :left "20%"
                    :right "20%"
                    :top "35px"}
            :class "border-solid border-black border divide-solid divide-black divide"}
      (for [{:keys [value on-click] :as option} options]
        ^{:key (str option)} [:div {:class "p-1 hover:bg-blue-300 hover:text-white cursor-pointer"
                                    :on-mouse-down #(when (fn? on-click) (on-click % option))} value])]]))

(defn sortable-list [{:keys [on-change list-items]}]
  [:> ReactSortable {:list (clj->js list-items)
                     :setList #(on-change (js->clj % {:keywordize-keys true}))}
   (for [list-item list-items]
     ^{:key (:id list-item)}
     [:div {:style {:height "2em"
                    :font-size "16px"
                    :padding "4px 0"}
            :class "border divide-solid divide-black divide cursor-grab active:cursor-grabbing"}
      [:> EqualsIcon {:class "inline-block"
                      :style {:height "1em"
                              :padding "0 4px 0 2px"}}]
      [:span {:class "inline-block"} (:name list-item)]])])