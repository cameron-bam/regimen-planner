(ns state
  (:require
   [clojure.string :as str]
   [cognitect.transit :as transit]
   [reagent.core :as r]
   [time.transit-helper :refer [read-handlers write-handlers]]))

(defn hash->state []
  (try (transit/read (transit/reader :json {:handlers read-handlers}) (js/atob (str/replace-first js/window.location.hash #"#" "")))
       (catch js/Error e
         (js/console.error e (str "Couldn't read state " js/window.location.hash)) nil)))

(defn state->hash [state]
  (try (js/btoa (transit/write (transit/writer :json {:handlers write-handlers :transform transit/write-meta}) (deref state))) (catch js/Error e (js/console.error e (str "Couldn't encode state " (pr-str state))))))

(def state! (r/atom (hash->state)))

(-add-watch state! ::hash-watcher (fn [old-val new-val]
                                    (try (set! js/location.hash (state->hash new-val)) (catch js/Error e (js/console.error e)))))