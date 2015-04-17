(ns xn.library.date-utils
  (:require
    [xn.library :refer [max-by]]
    [cljs-time.core :as time :refer [year month day hour minute second milli]]
    [cljs-time.local :as ltime]
    [cljs-time.format :as ftime]
    cljs-time.extend ; this will add IEquiv to goog.Date*
    goog.date.UtcDateTime
    goog.date.DateTime
    cljs.reader)
  (:refer-clojure :exclude [second]))

(defn ->parse-date [& fs]
  (let [formatters (map #(cond (string? %) (ftime/formatter %)
                               (keyword? %) (ftime/formatters %)
                               :else %)
                        fs)]
    (fn [s]
      (cond
        (time/date? s) s
        s (if-let [r (first
                       (keep #(try (ftime/parse-local % s)
                                   (catch js/Error _))
                             formatters))]
            r
            (throw (ex-info (apply str "Unrecognized: '" s "'  Possible formats: " (interpose "; " (filter string? fs))) {:input s})))))))

(defn ->format-date [f]
  (let [formatter (cond (string? f) (ftime/formatter f)
                        (keyword? f) (ftime/formatters f)
                        :else f)]
    (fn [date]
      (when date
        (ftime/unparse-local formatter date)))))

(defn date->ms [d]
  (when d (.getTime d)))

(defn ms->date [i]
  (when i
    (doto (ltime/local-now) (.setTime i))))

(defn date->min [d]
  (when d (/ (date->ms d) 60000)))

(defn zero-pad [n]
  (if (<= 0 n 9) (str "0" n) (str n)))

(defn seconds->hours [s]
  (let [total (if (neg? s) (- s) s)
        hour (js/Math.floor (/ total 3600))
        mins (js/Math.floor (/ (mod total 3600) 60 ))
        secs (mod total 60)]
        (str
          (when (and (< s 60) (neg? s)) "-")
          (when-not (zero? hour) (str hour "h "))
          (str mins "m "))))

(defn format-minute [m]
  (let [hour (js/Math.floor (/ m 60))
        mer (if (< hour 12) " AM" " PM")
        h (mod hour 12)
        h (if (zero? h) 12 h)]
    (str h ":" (zero-pad (mod m 60)) mer)))

(extend-type goog.date.DateTime
  IPrintWithWriter
  (-pr-writer [d w opts]
    (-write w "#xn/local-time ")
    (-write w (str (.getTime d)))))

(defn local-time-from-reader [ms]
  (ms->date ms))

(cljs.reader/register-tag-parser! "xn/local-time" local-time-from-reader)

(defn on-or-before? [a b]
  (or (time/= a b)
      (time/before? a b)))

(defn minutes-from-midnight [start-time]
  (when start-time
    (let [midnight (time/at-midnight start-time)]

      (if (time/= start-time midnight)
        0
        (time/in-minutes (time/interval midnight start-time))))))

(defn utc? [d]
  (instance? goog.date.UtcDateTime d))

(defn local? [d]
  (instance? goog.date.DateTime d))

(defn should-be-utc [d]
  (if (local? d)
    (time/date-time (year d) (month d) (day d) (hour d) (minute d) (second d) (milli d))
    d))

(defn should-be-local [d]
  (if (utc? d)
    (time/local-date-time (year d) (month d) (day d) (hour d) (minute d) (second d) (milli d))
    d))

(defn at-hour [d h]
  (cond (local? d)
        (time/local-date-time (year d) (month d) (day d) h (minute d) (second d) (milli d))
        (utc? d)
        (time/date-time (year d) (month d) (day d) h (minute d) (second d) (milli d))))

(defn at-minute [d m]
  (cond (local? d)
        (time/local-date-time (year d) (month d) (day d) (hour d) m (second d) (milli d))
        (utc? d)
        (time/date-time (year d) (month d) (day d) (hour d) m (second d) (milli d))))

(defn date+time
  "Combine the date part of d with the time part of t (both are cljs-time date or date-time)"
  [d t]
  (cond (and d t) (time/local-date-time (year d) (month d) (day d)
                                        (hour t) (minute t) (second t) (milli t))
        d (time/local-date-time (year d) (month d) (day d))
        t (should-be-local (time/today-at (hour t) (minute t) (second t) (milli t)))))

(def max-date (max-by date->ms time/date?))

; Transit handlers

(def transit-verbose-format (ftime/formatter "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))

(deftype VerboseDateHandler []
  Object
  (tag [_ v] "t")
  (rep [_ v] (str (ftime/unparse-local transit-verbose-format v) "Z"))
  (stringRep [h v] (.rep h v)))

(deftype DateHandler []
  Object
  (tag [_ v] "m")
  (rep [_ v] (.valueOf v))
  (stringRep [h v] (str (.rep h v)))
  (getVerboseHandler [_ v] (VerboseDateHandler.)))

(def date-read-handlers
  {"m" #(ms->date (if (number? %) % (js/parseInt %)))
   "t" #(ftime/parse-local transit-verbose-format %)})

(def date-write-handlers
  {goog.date.UtcDateTime (DateHandler.)
   goog.date.DateTime (DateHandler.)})

(defn add-equiv-protocol! []
  (extend-protocol IEquiv
    goog.date.UtcDateTime
    (-equiv [d other]
      (and (instance? goog.date.Date other)
           (= (.getTime d) (.getTime other))))
    goog.date.DateTime
    (-equiv [d other]
      (and (instance? goog.date.Date other)
           (= (.getTime d) (.getTime other))))))
