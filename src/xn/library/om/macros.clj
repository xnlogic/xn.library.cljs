(ns xn.library.om.macros
  (:require [om.core :as om]))

(defmacro defn-component [n [cur owner opts] & body]
  (let [fname (symbol (str "-*" n))]
    `(do
       (defn- ~fname [~cur ~owner ~opts]
         ~@body)
       (defn ~n [cur# owner# opts#]
         (om/build ~fname cur# {:opts opts#})))))

(defmacro defmethod-component [n key [cur owner opts] & body]
  (let [fname (symbol (str "-*" n "-" (name key)))]
    `(do
       (defn- ~fname [~cur ~owner ~opts]
         ~@body)
       (defmethod ~n ~key [cur# owner# opts#]
         (om/build ~fname cur# {:opts opts#})))))
