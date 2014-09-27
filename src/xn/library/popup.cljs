(ns xn.library.popup
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [xn.library.om :refer [swap-state!]]))

(def ^{:private true} active-popups (atom {}))

(defn show [key el]
  (swap! active-popups assoc key el)
  nil)

(defn hide [key]
  (swap! active-popups dissoc key)
  nil)

(defn popup [owner el]
  (let [k (om/get-state owner ::key)
        k (if k
            k
            (let [k (gensym)]
              (swap-state! owner assoc ::key k)
              k))]
    (if el
      (show k el)
      (hide k))))


(defn- show-popups [cur owner]
  (reify
    om/IRender
    (render [_]
      (apply
        dom/div nil
        (vals cur)))))


(defn position-popup [owner parent popup]
  (let [parent (om/get-node owner parent)
        rect (.getBoundingClientRect parent)]
    #js {:position "absolute"
         :left (aget rect "left")
         :top (aget rect "bottom")}))


(defn init-popups [element]
  (om/root show-popups active-popups {:target element}))

