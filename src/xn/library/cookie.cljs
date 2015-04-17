(ns xn.library.cookie
  (:require [goog.string :as gstr])
  (:import goog.net.Cookies)
  (:refer-clojure :exclude [get set!]))

(defn cookies []
  (Cookies. js/document))

(defn get
  ([k] (get k nil))
  ([k not-found]
   (let [v (.get (cookies) (name k) ::not-found)]
     (cond (= ::not-found v) not-found
           (string? v) (gstr/urlDecode v)
           :else v))))

(defn set! [k v & {:keys [max-age path domain secure?]}]
  (let [c (cookies)
        k (name k)]
    (prn k)
    (if (.isValidName c k)
      (.set c k v (or max-age -1) (or path "/") domain (boolean secure?))
      (throw (ex-info (str "Invalid cookie name: " k) {k v})))))

(defn unset! [k & {:keys [path domain]}]
  (let [c (cookies)
        k (name k)]
      (.remove c k (or path "/") domain)))
