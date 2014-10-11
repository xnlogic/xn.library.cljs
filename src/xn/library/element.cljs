(ns xn.library.element
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))


(defn event-value
  ([e]
   (.. e -target -value))
  ([f e]
   (if f
     (f (event-value e))
     (event-value e))))


(defn button
  ([f body]
   (button nil f body))
  ([opts f body]
   (dom/button
     (clj->js (merge opts {:className (str "btn " (:className opts))
                           :onClick f}))
     body)))


(defn table
  "Basic structure:
     head-rows and rows are both a vector of vectors of cell contents.

   A 'cell contents' can be either something renderable or a 2 element vector
   of React opts and something renderable [{:className etc} renderable].

   Any head rows or row can be annotated with React opts like ^{:className etc} [row ...]"
  ([rows] (table nil nil rows))
  ([head-rows rows] (table nil head-rows rows))
  ([opts head-rows rows]
   (letfn [(get-opts [x]
             (when (vector? x) (clj->js (first x))))
           (meta-opts [x]
             (when (meta x) (clj->js (meta x))))
           (renderable [x]
             (if (vector? x) (second x) x))]
     (dom/table
       opts
       (when (seq head-rows)
         (apply
           dom/thead nil
           (map (fn [head-row]
                  (apply
                    dom/tr (meta-opts head-row)
                    (map (fn [c] (dom/th (get-opts c) (renderable c)))
                         head-row)))
                head-rows)))
       (when (seq rows)
         (apply
           dom/tbody
           nil
           (map
             (fn [row]
               (apply
                 dom/tr (meta-opts row)
                 (map (fn [c] (dom/td (get-opts c) (renderable c)))
                      row)))
             rows)))))))
