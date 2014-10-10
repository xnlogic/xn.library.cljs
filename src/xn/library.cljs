(ns xn.library)

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
