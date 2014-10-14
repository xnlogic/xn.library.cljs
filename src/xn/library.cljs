(ns xn.library
  (:require [clojure.string :refer [blank?]])
  (:import [goog.ui IdGenerator]))


(deftype Volatile [^:mutable data]
  IDeref
  (-deref [_] data)

  IReset
  (-reset! [_ new-value]
    (set! data new-value))

  ISwap
  (-swap! [o f] (set! data (f data)))
  (-swap! [o f a] (set! data (f data a)))
  (-swap! [o f a b] (set! data (f data a b)))
  (-swap! [o f a b xs] (set! data (apply f data a b xs))))


(defn volatile
  "Like atom but stripped of all unneccessary features. Meant for internal use within a function."
  [data]
  (Volatile. data))


(defn reduce! [f o c]
  (persistent! (reduce f (transient o) c)))


(defn grouped-by
  "A transducer that acts like (seq (group-by f coll))"
  [f]
  (fn [rf]
    (let [group (volatile (transient (array-map)))]
      (fn
        ([] (rf))
        ([result] (rf (reduce rf result (persistent! @group))))
        ([result x]
         (-swap! group (fn [g]
                        (let [k (f x)]
                          (if-let [v (g k)]
                            (assoc! g k (conj v x))
                            (assoc! g k [x])))))
         result)))))


(defn lasts-by
  "A transducer that accomplishes the following but more efficiently
   (->> coll
        (group_by f)
        (map (fn [[k vals]] (last vals))))"
  ([f]
   (fn [rf]
     (let [matches (volatile (transient (array-map)))]
       (fn
         ([] (rf))
         ([result] (rf (reduce rf result (vals (persistent! @matches)))))
         ([result x]
          (-swap! matches assoc! (f x) x)
          result)))))
  ([f coll]
   (sequence (lasts-by f) coll)))


(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))


(defn index-of [coll item]
  (loop [[x & xs] coll idx 0]
    (if (= x item)
      idx
      (recur xs (inc idx)))))


(defn max-by
  ([f]
   (max-by f (constantly true)))
  ([f valid?]
   (fn [& things]
     (let [index (->> things
                      (filter (every-pred some? valid?))
                      (group-by f))]
       (->> index keys (apply max) index first)))))


(defn parse-int [s]
  (let [value (js/parseInt s)]
    (when-not (js/isNaN value) value)))


(defn ensure-seq [x]
  (cond (sequential? x) x
        x [x]))


(defn blank->nil [s]
  (if (blank? s) nil s))
