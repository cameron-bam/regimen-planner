(ns regimen-planner
  (:require
   ["@heroicons/react/16/solid" :refer [ArrowsPointingInIcon
                                        ArrowsPointingOutIcon]]
   ["react-dom/client" :refer [createRoot]]
   [clojure.string :as str]
   [components :refer [plus-selector sortable-list]]
   [components.calendar :refer [calendar-month]]
   [components.regimen-event :refer [form-classes label-classes
                                     regimen-event-form render-state-tag
                                     render-state-tags span-classes]]
   [reagent.core :as r]
   [state :refer [state!]]
   [tick.core :as t]
   [time :refer [date-range date-seq]]))

(def default-events {:medication
                     {:type :medication
                      :editing? true
                      :event {:rx ""
                              :timing {:type :pill-selector :value #{}}
                              :related-treatment {:type :select :value "" :options :state-tag/treatment-names}
                              :exact/start {:type "date" :value (t/date (t/now)) :hidden? :state-tag/has-related-treatment?}
                              :exact/stop {:type "date" :value (t/date (t/now)) :hidden? :state-tag/has-related-treatment?}
                              :relative/start {:type "relative-time-period" :value #time/period "-P0D" :hidden? :state-tag/not-has-related-treatment?}
                              :relative/stop {:type "relative-time-period" :value #time/period "P0D" :hidden? :state-tag/not-has-related-treatment?}}}
                     :treatment
                     {:type :treatment
                      :event {:treatment ""
                              :start (t/date (t/now))
                              :occurrences {:type :number
                                            :value 1}
                              :frequency {:type :time-period
                                          :value #time/period "P14D"}
                              :date-overrides {}}
                      :editing? true}
                     :footnote
                     {:type :footnote
                      :editing? true
                      :event {:footnote {:type :textarea
                                         :value ""}}}})

(def field-order [:rx
                  :timing
                  :related-treatment
                  :exact/start
                  :exact/stop
                  :relative/start
                  :relative/stop
                  :footnote
                  :treatment
                  :start
                  :occurrences
                  :frequency
                  :date])

(defmethod render-state-tag :state-tag/treatment-names [_ state _]
  (into [{:value "" :display "None Selected"}]
        (comp (filter (comp #{:treatment} :type))
              (map #(hash-map :value (-> % meta :id) :display (-> % :event :treatment)))) (:events state)))

(defmethod render-state-tag :state-tag/has-related-treatment? [_ _ event]
  (-> event :related-treatment :value str/blank? not))

(defmethod render-state-tag :state-tag/not-has-related-treatment? [_ _ event]
  (-> event :related-treatment :value str/blank?))

(defmethod render-state-tag :state-tag/multiple-occurrences? [_ _ event]
  (-> event :occurrences :value int (<= 1)))

(defn rebuild-timings! [state]
  (let [new-timings (->> state
                         :events
                         (mapcat #(-> % :event :timing :value))
                         (map #(hash-map :id % :name %))
                         set)
        timings (->> state :timings (map #(select-keys % [:id :name])))]
    (assoc state :timings (into (vec (filter new-timings timings))
                                (filter (complement (set timings)))
                                new-timings))))

(defn remove-event [events event]
  (->> events
       (keep (fn [item] (when-not (= (-> item meta :id) (-> event meta :id))
                          (cond-> item
                            (and
                             (= :treatment (:type event))
                             (= (-> item :event :related-treatment :value) (-> event meta :id str)))
                            (assoc-in [:event :related-treatment :value] "")))))
       vec))

(defn override-treatment-date! [treatment-id occurrence-index new-date]
  (swap! state! update :events
         (fn [events]
           (mapv (fn [event]
                   (if (= (-> event meta :id) treatment-id)
                     (assoc-in event [:event :date-overrides occurrence-index] new-date)
                     event))
                 events))))

(defn reset-treatment-date! [treatment-id occurrence-index]
  (swap! state! update :events
         (fn [events]
           (mapv (fn [event]
                   (if (= (-> event meta :id) treatment-id)
                     (update-in event [:event :date-overrides] dissoc occurrence-index)
                     event))
                 events))))

(defn expand-treatments [{:keys [type occurrences start frequency date-overrides] :as event}]
  (let [treatment-id (:treatment-id event)
        event (dissoc event :occurrences :start :frequency :date-overrides)]
    (if (not= :treatment type)
      [event]
      (mapv (fn [idx computed-date]
              (assoc event
                     :date (get date-overrides idx computed-date)
                     :occurrence-index idx
                     :treatment-id treatment-id))
            (range)
            (take (int (:value occurrences)) (date-seq start (:value frequency)))))))

(defn expand-timing [event]
  (if-let [timing (seq (:value (:timing event)))]
    (mapv #(assoc event :timing %) timing)
    [(dissoc event :timing)]))

(defn expand-exact-dates [{:exact/keys [start stop] :as event}]
  (let [event (dissoc event :exact/start :exact/stop)
        start (when-not (:hidden? start) (:value start))
        stop (when-not (:hidden? stop) (:value stop))]
    (cond
      (and start stop) (mapv #(assoc event :date %) (date-range start (t/>> stop #time/period "P1D") #time/period "P1D"))
      start [(assoc event :date start)]
      stop [(assoc event :date stop)]
      :else [event])))

(defn expand-relative-dates [events {:relative/keys [start stop] :as event}]
  (let [event (dissoc event :relative/start :relative/stop)
        related-treatments (filter #(= (str (-> % meta :id)) (-> event :related-treatment :value)) events)]
    (if (seq related-treatments)
      (mapcat (fn [{% :date}]
                (let [start (when-not (or (nil? %) (:hidden? start)) (t/>> % (:value start)))
                      stop (when-not (or (nil? %) (:hidden? stop)) (t/>> % (:value stop)))]
                  (prn [% start stop])
                  (cond
                    (and start stop) (mapv (fn [%] (assoc event :date %)) (date-range start (t/>> stop #time/period "P1D") #time/period "P1D"))
                    start [(assoc event :date start)]
                    stop [(assoc event :date stop)]
                    :else [event]))) related-treatments)
      [event])))

(defn expand-medications [events event]
  (if (= :medication (:type event))
    (->> [event]
         (mapcat expand-timing)
         (mapcat expand-exact-dates)
         (mapcat (partial expand-relative-dates events))
         (filter :date)
         vec)
    [event]))

(defn expand-events []
  (let [events (->> @state!
                    :events
                    (map #(update % :event (partial render-state-tags @state!)))
                    (map (fn [event] (assoc (merge event (:event event))
                                            :treatment-id (-> event meta :id))))
                    (mapcat expand-treatments))]
    (mapcat (partial expand-medications events) events)))

(defn date-renderer [{:keys [title events timing-order treatment-id occurrence-index date]}]
  (r/with-let [editing-date? (r/atom false)]
    (let [event-groups (group-by :timing events)]
      [:div {:class "p-2"}
       [:div {:class "pt-1 pb-3.5 pr-4 font-bold leading-none"}
        (if (and title treatment-id)
          [:div
           [:span {:class "cursor-pointer hover:underline"
                   :on-click #(reset! editing-date? true)}
            title]
           (when @editing-date?
             [:div {:class "print-hidden mt-1"}
              [:input {:type "date"
                       :value (str date)
                       :class "text-sm rounded border-gray-300"
                       :on-change (fn [e]
                                    (let [new-date (t/date (-> e .-target .-value))]
                                      (override-treatment-date! treatment-id occurrence-index new-date)
                                      (reset! editing-date? false)))}]
              [:button {:class "text-xs ml-1 text-gray-500 hover:text-red-500"
                        :on-click (fn [e]
                                    (.stopPropagation e)
                                    (reset-treatment-date! treatment-id occurrence-index)
                                    (reset! editing-date? false))}
               "Reset"]])]
          (or title [:br]))]
       (when events
         (into [:div {:class "event-list"}]
               (comp
                (keep #(when-let [events (get event-groups %)] [% events]))
                (map (fn [[timing events]]
                       (into [:section {:class "mb-3"} (when timing [:p {:class "font-medium mb-0.5 leading-none"} (str (str/trim timing) ":")])]
                             (map (fn [{:keys [rx]}] [:p {:class "mt-0 mb-0.5 ml-0.5 leading-none"} rx]))
                             events))))
               timing-order))])))

(defn main []
  (conj
   (if (:full-screen? @state!)
     [:<>]
     [:div {:class "h-screen w-screen"
            :style {:display "grid"
                    :grid-template-rows "4rem 1fr"
                    :grid-template-columns "320px 1fr"}}
      [:div {:class "relative border-b-2 border-grey-500"
             :style {:grid-column "1 / span 2"}}
       [:h1 {:class "font-semibold text-4xl absolute bottom-3 left-3"} "Regimen Planner"]]
      [:div {:class "border-r-2 border-grey-500 overflow-scroll"}
       (when-let [timings (-> @state! :timings seq)]
         [:form {:class form-classes}
          [:label {:class label-classes}
           [:span {:class span-classes} "Timing Order"]
           [sortable-list {:on-change #(swap! state! assoc :timings %)
                           :list-items timings}]]])
       (for [event (:events @state!)]
         (let [swap-state! (fn [& args]
                             (swap! state! update :events (fn [events]
                                                            (mapv (fn [event-to-update]
                                                                    (if (= (-> event meta :id) (-> event-to-update meta :id))
                                                                      (apply (first args) event-to-update (rest args))
                                                                      event-to-update)) events))))]
           ^{:key (-> event meta :id)}
           [regimen-event-form
            (merge
             event
             {:toggle-edit #(swap-state! update :editing? not)
              :on-change (fn [new-event]
                           (swap-state! (fn [old-event]
                                          (let [old (:event old-event)
                                                series-changed? (and (= :treatment (:type old-event))
                                                                     (or (not= (:start old) (:start new-event))
                                                                         (not= (:occurrences old) (:occurrences new-event))
                                                                         (not= (:frequency old) (:frequency new-event))))
                                                has-overrides? (seq (:date-overrides old))
                                                clear-overrides? (and series-changed?
                                                                      (or (not has-overrides?)
                                                                          (js/confirm "Treatment schedule changed. Clear manual date overrides?")))]
                                            (update old-event :event merge
                                                    (when (or (not series-changed?) clear-overrides?) new-event)
                                                    (when clear-overrides? {:date-overrides {}})))))
                           (swap! state! rebuild-timings!))
              :drop-event! #(do
                              (swap! state! update :events remove-event event)
                              (swap! state! rebuild-timings!))
              :field-order field-order})]))
       [plus-selector
        {:value "Add Treatment"
         :on-click #(swap! state! update :events conj (with-meta (:treatment default-events) {:id (random-uuid)}))}
        {:value "Add Medication"
         :on-click #(swap! state! update :events conj (with-meta (:medication default-events) {:id (random-uuid)}))}
        {:value "Add Footnote"
         :on-click #(swap! state! update :events conj (with-meta (:footnote default-events) {:id (random-uuid)}))}]]])


   [:div {:class (str "hover-target h-full relative " (when-not (:full-screen? @state!) "overflow-scroll"))}
    [:button {:on-click #(swap! state! update :full-screen? not)
              :class "hover:bg-gray-300 parent-hover-visible absolute flex rounded-full justify-center items-center print-hidden" ;; rounded-full
              :style {:height "30px" :width "30px" :top "5px" :right "5px"}}
     [:> (if (:full-screen? @state!) ArrowsPointingInIcon ArrowsPointingOutIcon) {:style {:height "60%" :width "60%"}}]]
    (let [[date-data footnotes] (when-let [events (seq (expand-events))]
                                  [(group-by (comp t/year-month first)
                                             (reduce (fn [acc {:keys [date] :as event}]
                                                       (case (:type event)
                                                         :medication (if (get-in acc [date :events])
                                                                       (update-in acc [date :events] conj event)
                                                                       (update acc date merge {:events [event] :timing-order (into [nil] (map :id) (-> @state! :timings))}))
                                                         :treatment (update acc date merge {:title (:treatment event)
                                                                                            :treatment-id (:treatment-id event)
                                                                                            :occurrence-index (:occurrence-index event)
                                                                                            :date date})
                                                         acc))
                                                     {}
                                                     events))
                                   (->> events
                                        (filter (comp #{:footnote} :type))
                                        (map #(get-in % [:event :footnote :value])))])]
      [:<>
       [:div {:class "p-12"}
        (if date-data
          (into [:<>]
                (map-indexed
                 (fn [i [_ events]]
                   ^{:key i} [calendar-month {:date-renderer date-renderer
                                              :date-data (into {} events)
                                              :footnotes footnotes}]))
                (sort-by first date-data))
          [calendar-month])]])]))

(defonce root (createRoot (js/document.getElementById "root")))

(defn init []
  (.render root (r/as-element [main])))

(defn ^:dev/after-load re-render []
  (init))