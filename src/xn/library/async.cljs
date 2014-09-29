(ns xn.library.async
  (:require [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.channels :as channels]
            [cljs.core.async :refer [put! >! <! close! chan timeout alts!
                                     map< filter< mapcat> mapcat<]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [xn.library.async :refer [go-loop go-loop-alts]])
  (:refer-clojure :exclude [repeat]))

(defn when-chan-closed [f in]
  (let [out (chan)]
    (go (loop []
          (if-let [m (<! in)]
            (do
              (>! out m) (recur))
            (do
              (f) (close! out)))))
    out))

(defn take-for [msecs in]
  (let [cs [in (timeout msecs)]]
    (go-loop-alts in out [] [[m c] cs]
                  (when m
                    (do (>! out m) (recur))))))

(defn cap
  "Like dorun for a channel"
  [in]
  (go (while (<! in))))

(defn tap-until
  ([pred in] (tap-until pred in (chan 1)))
  ([pred in out]
   (go-loop in out [] m
            (put! out m)
            (if (pred m) m (recur)))))

(defn repeat [v]
  (let [out (chan)]
    (go (while true (>! out v)))
    out))

(defn debounce [msecs keep-init keep-debounced in & opts]
  (let [opts (apply array-map opts)
        clear (or (:clear opts) (chan))
        out (chan)]
    (go
      (loop [state ::init cs [in] debounced nil]
        (let [[_ threshold] cs
              cs (conj cs clear)
              [v source] (alts! cs)]
          (condp = source
            in (if v
                 (condp = state
                   ::init
                   (do
                     (when keep-init (>! out v))
                     (recur ::debouncing [in (timeout msecs)] (when-not keep-init v)))
                   ::debouncing
                   (recur state [in (timeout msecs)] (when keep-debounced v)))
                 (if debounced
                   (recur state [threshold] debounced)
                   (close! out)))
            clear (recur ::init [in] nil)
            (do (when debounced (>! out debounced))
                (recur ::init [in] nil))))))
    out))

(defn after-pause [message msecs]
  (fn [in]
    (go-loop-alts
      in out [cs [in]] [[m sc] cs]
      (if (= in sc)
        (if m
          (when (>! out m)
            (recur [in (timeout msecs)]))
          (close! out))
        (when (>! out message)
          (recur [in]))))))

(defn when-chan
  "get-chan is a fn that takes the current message and either:
   - returns a channel that should be closed or have data pushed
     to it which will be ignored (externally to this fn) when the
     predicate is satisfied; or
   - returns nil which means that the current message will be
     discarded"
  [get-chan in]
  (let [out (chan)]
    (go
      (loop [cs [in] value nil]
        (let [[v c] (alts! cs) ]
          (if (= c in)
            (when v
              (if-let [pred-chan (get-chan v)]
                (recur [pred-chan in] v)
                (recur [in] v)))
            (when (>! out value)
              (recur [in] nil))))))
    out))

; TODO: update to current core.async version.
#_(defn delay-chan*
  "chan-thunk is something that when deref'd returns a channel. The
   first thing that that channel emits will be saved and always returned
   by the channel this produces."
  [chan-thunk]
  (let [state (atom ::start)
        c (reify
            impl/Channel
            (close! [_]
              (reset! state nil))
            impl/ReadPort
            (take! [_ fn1]
              (let [data @state]
                (cond
                  (= ::start data)
                  (let [ret
                        (impl/take! @chan-thunk
                                    (reify
                                      impl/Handler
                                      (active? [_] (impl/active? fn1))
                                      (lock-id [_] (impl/lock-id fn1))
                                      (commit [_]
                                        (let [f1 (impl/commit fn1)]
                                          (fn [result]
                                            (reset! state result)
                                            (f1 result))))))]
                    (if (and ret (not (nil? @ret)))
                      (channels/box @ret)
                      ret))
                  data (channels/box data)
                  :else nil))))]
    c))

