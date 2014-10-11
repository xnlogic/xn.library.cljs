(ns xn.library
  (:import [goog.ui IdGenerator]))


(defn reduce! [f o c]
  (persistent! (reduce f (transient o) c)))


(defn grouped-by
  "A transducer that acts like (seq (group-by f coll))"
  [f]
  (fn [rf]
    (let [group (atom (array-map))]
      (fn
        ([] (rf))
        ([result] (rf (reduce rf result @group)))
        ([result x]
         ; TODO use update on next cljs version upgrade
         (swap! group update-in [(f x)] (fn [v] (if v (conj v x) [x])))
         result)))))


(defn lasts-by
  "A transducer that accomplishes the following but more efficiently
   (->> coll
        (group_by f)
        (map (fn [[k vals]] (last vals))))"
  ([f]
   (fn [rf]
     (let [matches (atom (array-map))]
       (fn
         ([] (rf))
         ([result] (rf (reduce rf result (vals @matches))))
         ([result x]
          (swap! matches assoc (f x) x)
          result)))))
  ([f coll]
   (sequence f coll)))


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
  (if (re-find #"^\s*$" s) nil s))
