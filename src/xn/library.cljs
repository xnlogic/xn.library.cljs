(ns xn.library
  (:require [clojure.string :refer [blank?] :as s])
  (:import [goog.ui IdGenerator]))


(defn reduce! [f o c]
  (persistent! (reduce f (transient o) c)))

(defn reduce-kv! [f o c]
  (persistent! (reduce-kv f (transient o) c)))

(defn dissoc-in
  "Disassociates a value in a nested associative structure, where ks is a
   sequence of keys and returns a new nested structure.  If any levels do not
   exist, hash-maps will be created. If a resulting map is empty, dissoc it,
   too, so that you can remove a whole chain of single-key maps in one
   statement."
  [m [k & ks]]
  (let [f #(if (and (nil? %) (or (map? m) (nil? m)))
             (dissoc m k)
             (assoc m k %))
        m (if ks
            (f (dissoc-in (get m k) ks))
            (f nil))]
    (when (seq m) m)))


(defn grouped-by
  "A transducer that acts like (seq (group-by f coll))"
  [f & {:keys [keys?] :or {keys? true}}]
  (fn [rf]
    (let [group (volatile! (transient (array-map)))]
      (fn
        ([] (rf))
        ([result]
         (rf
           (if keys?
             (reduce rf result (persistent! @group))
             (reduce rf result (vals (persistent! @group))))))
        ([result x]
         (vswap! group (fn [g]
                         (let [k (f x)]
                           (if-let [v (get g k)]
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
     (let [matches (volatile! (transient (array-map)))]
       (fn
         ([] (rf))
         ([result] (rf (reduce rf result (vals (persistent! @matches)))))
         ([result x]
          (vswap! matches assoc! (f x) x)
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


(defn index-by [f coll]
  (reduce (fn [idx el] (assoc idx (f el) el))
          {}
          coll))

(defn parse-int [s]
  (let [value (js/parseInt s)]
    (when-not (js/isNaN value) value)))


(defn ensure-seq [x]
  (cond (sequential? x) x
        x [x]))


(defn blank->nil [s]
  (when-not (blank? s) s))


(defn string->regex [s]
  (when-not (blank? s)
    (re-pattern (str "(?i)" (s/replace s #"\s+" ".*")))))

(defn juxt-some [& fs]
  (when-let [fs (seq (remove nil? fs))]
    (apply juxt fs)))

(defn comp-some [& fs]
  (when-let [fs (seq (remove nil? fs))]
    (apply comp fs)))
