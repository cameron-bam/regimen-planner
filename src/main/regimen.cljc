(ns regimen
  (:require
   [clojure.walk :refer [postwalk]]
   [tick.core :as t]
   [time :refer [->days]]))

#?(:clj (defn resolve-op [op]
          (let [op-sym (symbol (namespace op) (name op))]
            (resolve op-sym))))

#?(:cljs (defn resolve-op [op]
           (case op
             ::t/>> t/>>
             (throw (ex-info "Cannot resolve op!" {:op op})))))

(def regimens
  {"tchp-chemo"
   {:timing-order [nil :AM :PM]
    :events
    [{:medication "Dexamethasone"
      :instructions "8mg (2 tablets)"
      :timing [:AM :PM]
      :duration #time/period "P3D"
      :start ^{:apply ::t/>>} ['treatment-date #time/period "P-1D"]}
     {:medication "Olanzapine"
      :instructions "(1 tablet)"
      :timing [:PM]
      :duration #time/period "P4D"
      :start 'treatment-date}
     {:medication "Claritin"
      :timing [:AM]
      :start 'treatment-date
      :duration #time/period "P7D"}]}

   "Taxol/Carboplatin"
   {:timing-order [nil :Bedtime]
    :events
    [{:medication "Dexamethasone"
      :instructions "20mg (5 tablets) - 12 hours before treatment"
      :start ^{:apply ::t/>>} ['treatment-date #time/period "P-1D"]}
     {:medication "Dexamethasone"
      :instructions "20mg (5 tablets) - 6 hours before treatment"
      :start 'treatment-date}
     {:medication "Olanzapine"
      :timing [:Bedtime]
      :instructions "2.5mg (1 tablet)"
      :start 'treatment-date
      :duration #time/period "P4D"}]}

   "Docetaxel/Cyclophosphamide"
   {:events
    [{:medication "Dexamethasone"
      :instructions "8mg (2 tablets)"
      :start ^{:apply ::t/>>} ['treatment-date #time/period "P-1D"]
      :duration #time/period "P3D"}
     {:medication "Claritin"
      :instructions "(1 tablet)"
      :start 'treatment-date
      :duration #time/period "P7D"}]}

   "Taxol/Herceptin"
   {:events
    [{:medication "Dexamethasone"
      :instructions "20mg (5 tablets) - 12 hours before treatment"
      :start ^{:apply ::t/>>} ['treatment-date #time/period "P-1D"]}
     {:medication "Dexamethasone"
      :instructions "20mg (5 tablets) - 6 hours before treatment"
      :start 'treatment-date}]}

   "Pembrolizumab/Doxorubicin/Cyclophosphamide (Pembro/AC)"
   {:events
    [{:medication "Olanzapine"
      :instructions "2.5mg (1 tablet)"
      :start 'treatment-date
      :duration #time/period "P4D"}]}})

(defn render-regimen [{:keys [regimen] :as input}]
  (let [regimen-template (postwalk (fn [form]
                                     (cond
                                       (symbol? form)
                                       (get input (-> form name keyword))

                                       (:apply (meta form))
                                       (try
                                         (apply (resolve-op (:apply (meta form))) form)
                                         (catch #?(:clj Throwable :cljs js/Error) _ nil))

                                       :else form)) (get regimens regimen))]
    (cond-> regimen-template
      (not (:timing-order regimen-template))
      (assoc :timing-order [nil]))))

(defn expand-events [{:keys [start duration timing] :as order}]
  (mapcat (fn [date]
            (map (fn [timing]
                   (-> order
                       (dissoc :start :duration)
                       (assoc :timing timing :date date))) (or (seq timing) [nil]))) (->days start (or duration #time/period "P1D"))))

(comment
  (-> (render-regimen {:regimen "tchp-chemo" :treatment-date (t/instant)})
      (update :events (partial mapcat expand-events))))