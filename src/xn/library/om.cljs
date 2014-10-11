(ns xn.library.om
  (:require [om.core :as om]))

(def cursor? om/cursor?)

(defn value [x]
  (if (satisfies? om/IValue x)
    (om/-value x)
    x))

(defn swap-state! [owner f & opts]
  (om/update-state! owner (fn [state] (apply f state opts))))


(defn node-width [owner name]
  (-> (om/get-node owner name) gstyle/getSize (.-width)))

