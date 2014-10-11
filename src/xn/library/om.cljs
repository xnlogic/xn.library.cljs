(ns xn.library.om
  (:require [om.core :as om]
            [goog.style :as gstyle]))

(def cursor? om/cursor?)

(defn value [x]
  (if (satisfies? om/IValue x)
    (om/-value x)
    x))

(defn swap-state! [owner f & opts]
  (om/update-state! owner (fn [state] (apply f state opts))))


(defn node-width [owner name]
  (-> (om/get-node owner name) gstyle/getSize (.-width)))

(defn test-env? []
  (boolean (re-find #"PhantomJS" js/navigator.userAgent)))

