(ns xn.library.om
  (:require [om.core :as om]
            [goog.style :as gstyle]
						[cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def cursor? om/cursor?)

(defn value [x]
  (if (satisfies? om/IValue x)
    (om/-value x)
    x))

(defn swap-state! [owner f & opts]
  (om/update-state! owner (fn [state] (apply f state opts))))

(defn temp-state! [owner k value]
  (go
    (let [id (gensym)
          id-key (keyword "temp-state" k)]
      (swap-state! owner assoc id-key id k value)
      (<! (async/timeout 5000))
      (when (= id (om/get-state owner id-key))
        (swap-state! owner dissoc id-key k)))))


(defn node-width [owner name]
  (-> (om/get-node owner name) gstyle/getSize (.-width)))

(defn test-env? []
  (boolean (re-find #"PhantomJS" js/navigator.userAgent)))

