(ns xn.library.sort
  (:require [clojure.string :as s :refer [capitalize blank?]]))


(defn nil>
  "Wrap a comparison function so that it puts nils at the end"
  [compare]
  (fn [a b]
    (cond (= a b) 0
          (nil? a) -1
          (nil? b) 1
          :else (compare a b))))


(defn =e=
  "Wrap a comparison function so that if exception treat as equal"
  [compare]
  (fn [a b]
    (try (compare a b)
         (catch js/Error e 0))))


(defn rev
  "Wrap a comparison function and reverse it"
  [compare]
  (comp (partial * -1) compare))


(defn- compare-parseInt [a b]
  (if (blank? a)
    (if (blank? b) 0 -1)
    (if (blank? b)
      1
      (compare (js/parseInt a) (js/parseInt b)))))


(defn compare-numbered* [a b]
  (reduce (fn [_ [[_ a-s a-n] [_ b-s b-n]]]
            (let [s (compare a-s b-s)]
              (if (zero? s)
                (let [s (compare-parseInt a-n b-n)]
                  (if (zero? s)
                    s
                    (reduced s)))
                (reduced s))))
          0
          (map vector
               (concat (when (string? a) (re-seq #"(\D*)(\d*)" a)) [nil])
               (concat (when (string? b) (re-seq #"(\D*)(\d*)" b)) [nil]))))


(def compare-ascending (nil> (=e= compare)))
(def compare-numbered (nil> (=e= compare-numbered*)))


(defn compare-descending [a b]
  ((comp #(* -1 %) compare-ascending) a b))


(defn record-comparator
  "with is a map {field name -> comparator function}"
  ([x] (if (map? x)
         (record-comparator (:order x [:display_name]) x)
         (record-comparator x {})))
  ([fields {:keys [reverse with]}]
   (fn [a b]
     (loop [[k & ks] fields]
       (if k
         (let [c (or (get with k) compare-numbered)
               c (if (get reverse k) (comp - c) c)
               ord (c (get a k) (get b k))]
           (if (zero? ord) (recur ks) ord))
         0)))))



