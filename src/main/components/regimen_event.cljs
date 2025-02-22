(ns components.regimen-event
  (:require
   ["@heroicons/react/16/solid" :refer [CheckIcon PencilIcon TrashIcon XMarkIcon]]
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]
   [state :refer [state!]]
   [reagent.core :as r]
   [tick.core :as t]))

(def form-classes "px-5 pb-5 border-b border-gray-300 border-solid")
(def display-classes "py-7 px-5 border-b border-gray-300 border-solid")
(def input-classes "mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-300 focus:ring focus:ring-indigo-200 focus:ring-opacity-50")
(def label-classes "block pt-4")
(def span-classes "text-700")

(defn regimen-input-on-change [opts event]
  (let [value (-> event .-target .-value)
        validator (or (:validator opts) (constantly true))]
    (when-let [on-change (:on-change opts)]
      (when (validator value)
        (on-change value)))))

(defmulti regimen-event-input (fn [{:keys [type]} & _] (name type)))

(defmethod regimen-event-input :default [opts]
  [:label {:class label-classes}
   [:span {:class span-classes} (:label opts)]
   [:input (assoc opts :class input-classes :on-change (partial regimen-input-on-change opts))]])

(defmethod regimen-event-input "select" [opts]
  [:label {:class label-classes}
   [:span {:class span-classes} (:label opts)]
   [:select (-> opts (assoc :class input-classes :on-change (partial regimen-input-on-change opts)) (dissoc :options))
    (for [option (:options opts)]
      (let [{:keys [display value]} (if (map? option) option {:display option :value option})]
        ^{:key value} [:option {:value value} display]))]])

(defmethod regimen-event-input "pill-selector" [{:keys [label on-change value]}]
  (r/with-let [pill-input (r/atom "")]
    [:label {:class label-classes}
     [:span {:class span-classes} label]
     [:input {:class input-classes
              :name "pill-input"
              :value @pill-input
              :on-change #(reset! pill-input (-> % .-target .-value))
              :on-key-press #(let [key-code (.-key %)]
                               (when (= "Enter" key-code)
                                 (when-not (str/blank? @pill-input)
                                   (on-change (conj value @pill-input))
                                   (reset! pill-input "")
                                   (.preventDefault %))))}]
     (when (seq value)
       [:div {:class "py-2"}
        (for [item value]
          ^{:key item}
          [:div {:class "gray-300 border-solid border-gray-300 border rounded-md inline-block mr-2 overflow-hidden"}
           [:span {:class "px-2 py-1"} item]
           [:span {:class "bg-gray-300 px-2 py-1 h-full inline-block hover:cursor-pointer"
                   :on-click #(on-change (disj value item))}
            [:> XMarkIcon {:style {:height "1em" :display "inline-block"}}]]])])]))

(defmethod regimen-event-input "time-period" [{:keys [label on-change value validator]}]
  (r/with-let [period-value (r/atom (t/days value))]
    (let [validator (or validator #(>= % 0))
          inner-on-change #(on-change (t/new-period @period-value :days))]
      [:label {:class label-classes}
       [:span {:class span-classes} label]
       [:div {:class "flex items-center"}
        [:input {:class input-classes
                 :type "number"
                 :on-change #(let [value (-> % .-target .-value int)]
                               (when (validator value)
                                 (reset! period-value value)
                                 (inner-on-change)))
                 :value @period-value}]
        [:span {:class "px-2 grow"} "days"]]])))


(defmethod regimen-event-input "relative-time-period" [{:keys [label on-change value validator]}]
  (r/with-let [period-value (r/atom {:days (abs (t/days value))
                                     :sign (if (> (t/days value) 0) "after" "before")})]
    (let [inner-on-change #(on-change (t/new-period (* (:days @period-value) ({"after" 1 "before" -1} (:sign @period-value))) :days))
          validator (or validator #(>= % 0))]
      [:label {:class label-classes}
       [:span {:class span-classes} label]
       [:div {:class "flex items-center"}
        [:input {:class input-classes
                 :type "number"
                 :on-change #(let [value (-> % .-target .-value)]
                               (when (validator value)
                                 (swap! period-value assoc :days (-> % .-target .-value))
                                 (inner-on-change)))
                 :value (:days @period-value)}]
        [:span {:class "px-2"} "days"]
        [:select {:class input-classes
                  :on-change #(do
                                (swap! period-value assoc :sign (-> % .-target .-value))
                                (inner-on-change))
                  :value (:sign @period-value)}
         [:option "after"]
         [:option "before"]]]])))

(defmulti regimen-event-display (fn [{:keys [type]} & _] (name type)))

(defn period->string [period]
  (let [days (t/days period)
        abs-days (abs days)]
    (str days " day" (when (not= abs-days 1) "s"))))

(defn relative-period->string [period]
  (let [days (t/days period)
        abs-days (abs days)]
    (if (= 0 days)
      "day of treatment"
      (str abs-days " day" (when (not= abs-days 1) "s") " " (if (< days 0) "before" "after")))))

(defmethod regimen-event-display :default [{:keys [label value]}]
  [:p [:span label] ": " [:span (str (cond->> value
                                       (coll? value) (str/join ", ")
                                       (t/period? value) period->string))]])

(defmethod regimen-event-display "relative-time-period" [{:keys [label value]}]
  [:p [:span label] ": " [:span (relative-period->string value)]])

(defmethod regimen-event-display "select" [{:keys [label value options]}]
  (prn [value options])
  [:p [:span label] ": " [:span (or (some #(when (= (str value) (str (:value %))) (:display %)) options) value)]])

(defmulti render-state-tag (fn [val _ _] val))

(defmethod render-state-tag :default [v _ _] v)

(defn render-state-tags [state event]
  (postwalk (fn [form] (try (render-state-tag form state event)
                            (catch js/Error e (js/console.error e "Couldn't expand state tags") form))) event))

(defn regimen-event-form [{:keys [on-change editing? drop-event! event toggle-edit field-order]}]
  (r/with-let [form-state (r/atom event)]
    (when-not (or editing? (= @form-state event))
      (reset! form-state event))
    (let [actions [(when editing?
                     {:icon CheckIcon
                      :id "save"
                      :type "submit"
                      :on-click #(do
                                   (.preventDefault %)
                                   (.stopPropagation %)
                                   (on-change @form-state)
                                   (toggle-edit))})
                   (when-not editing?
                     {:icon PencilIcon
                      :id "edit"
                      :type "button"
                      :on-click toggle-edit})
                   {:icon TrashIcon
                    :id "delete"
                    :type "button"
                    :on-click drop-event!}]]
      [:div {:class "relative hover-target"}
       [(if editing? :form :ul) {:class (if editing? form-classes display-classes)}
        [:div {:class (str "absolute flex " (when-not editing? "parent-hover-visible"))
               :style {:top "5px" :right "5px"}}
         (for [{:keys [icon id type on-click]} (filter some? actions)]
           ^{:key id}
           [:button {:on-click on-click ;; todo - test
                     :type type
                     :class "hover:bg-gray-300 flex rounded-full justify-center items-center" ;; rounded-full
                     :style {:height "30px" :width "30px"}}
            [:> icon {:style {:height "60%" :width "60%"}}]])]
        (into
         [:<>]
         (filter some?)
         (for [[field-key field] (let [form-state* (render-state-tags @state! @form-state)]
                                   (map #(vector % (get form-state* %)) field-order))]
           (let [type (cond
                        (map? field) (:type field)
                        (t/date? field) "date"
                        :else "text")
                 opts (cond-> {:type type
                               :label (str/replace (str/capitalize (name field-key)) #"-" " ")}
                        (map? field) (merge (dissoc field :hidden?))
                        (not (map? field)) (assoc :value field))]
             (when (and
                    (some? field)
                    (not (and (map? field) (:hidden? field))))
               (with-meta
                 (if editing?
                   [regimen-event-input (assoc opts :on-change #(swap! form-state update field-key (fn [old-val new-val]
                                                                                                     (cond->> new-val
                                                                                                       (= "date" type) t/date
                                                                                                       (map? old-val) (assoc old-val :value)))
                                                                       %))]
                   [:li [regimen-event-display opts]])
                 {:key field-key})))))]])))