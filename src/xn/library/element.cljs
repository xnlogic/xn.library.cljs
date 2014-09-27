(ns xn.library.element
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))


(defn button
  ([f body]
   (button nil f body))
  ([opts f body]
   (dom/button
     (clj->js (merge opts {:className (str "btn " (:className opts))
                           :onClick f}))
     body)))


(defn table
  "Basic structure: head-row is a vector of cell contents. Rows is a vector of
   vectors of cell contents.

   A cell contents can be either something renderable or a 2 element vector of
   React opts and something renderable [{:className etc} renderable].

   Rows can be annotated with React opts like ^{:classname etc} [row ...]"
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
