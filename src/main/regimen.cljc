(ns regimen
  (:require [tick.core :as t]
            [time :refer [->days]]
            [clojure.walk :refer [postwalk]]))

#?(:clj (defn resolve-op [op]
          (let [op-sym (symbol (namespace op) (name op))]
            (resolve op-sym))))

#?(:cljs (defn resolve-op [op]
           (case op
             ::t/>> t/>>
             (throw (ex-info "Cannot resolve op!" {:op op})))))

(defn render-regimen [{:keys [regimen] :as input}]
  (let [regimens {:tchp-chemo [{:medication "Dexamethasone"
                                :dose [2 :tablets]
                                :volume [8 :milligrams]
                                :timing [:breakfast :dinner]
                                :duration #time/period "P3D"
                                :start ^{:apply ::t/>>} ['treatment-date #time/period "P-1D"]}
                               {:medication "Olanzapine"
                                :dose [1 :tablet]
                                :timing [:night]
                                :duration #time/period "P4D"
                                :start 'treatment-date}
                               {:medication "Claritin"
                                :timing [:morning]
                                :start 'treatment-date
                                :duration #time/period "P7D"}]}]
    (postwalk (fn [form]
                (cond
                  (symbol? form)
                  (get input (-> form name keyword))

                  (:apply (meta form))
                  (try
                    (apply (resolve-op (:apply (meta form))) form)
                    (catch #?(:clj Throwable :cljs js/Error) _ nil))

                  :else form)) (get regimens regimen))))

(defn expand-events [{:keys [start duration timing] :as order}]
  (mapcat (fn [date]
            (map (fn [timing]
                   (-> order
                       (dissoc :start :duration)
                       (assoc :timing timing :date date))) timing)) (->days start duration)))

(def get-regimen-events (comp (partial sort-by :date) (partial mapcat expand-events) render-regimen))

(comment
  (->> (render-regimen {:regimen :tchp-chemo :treatment-date (t/instant)})
       (mapcat expand-events)))