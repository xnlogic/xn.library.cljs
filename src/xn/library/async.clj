(ns xn.library.async)

(defmacro go-loop
  "Puts a loop around the channel consumption.
   Creates an out-chan and closes it when the in-chan closes.

   Usage:

   (defn chan-counter [in]
     (go-loop in out [counter 0] chan-data
              (>! out chan-data)
              (recur (inc counter))))"
  [in-chan out-chan-binding loop-bind bound & body]
  `(let [in-chan# ~in-chan
         ~out-chan-binding (cljs.core.async/chan)]
     (cljs.core.async.macros/go
       (loop ~loop-bind
         (when-let [~bound (~'<! in-chan#)]
           ~@body))
       (cljs.core.async/close! ~out-chan-binding))
     ~out-chan-binding))

(defmacro go-loop-alts
  "Will close the out chan if there are no channels in chans, or if there
   is only one channel in 'chans' and it gets closed, or if the loop falls through.

   Usage:

   in: pass a channel
   out: chan is created.
   loop-bind: bindings same as for a loop
   [bound chans]: as in (let [bound (alts! chans] ...)
   body: multiple lines. Called only if the last channel is not closed

   (defn simplified-after-pause [msecs in]
     (go-loop-alts
       in out [chans [in] data nil] [[chan-data selected-chan] chans]
       (cond (and (= in selected-chan) chan-data (>! out m))
             (recur [in (timeout msecs)] chan-data)
             data  (>! out data)
             :else (recur [in] nil))))"

  [in-chan out-chan-binding loop-bind [bound chans] & body]
  `(let [in-chan# ~in-chan
         ~out-chan-binding (cljs.core.async/chan)]
     (cljs.core.async.macros/go
       (loop ~loop-bind
         (when (seq ~chans)
           (let [~bound (~'alts! ~chans)]
             (when (or (< 1 (count ~chans))
                       ~(if (vector? bound) (first bound) `(first ~bound)))
               (do ~@body)))))
       (cljs.core.async/close! ~out-chan-binding))
     ~out-chan-binding))

(defmacro <?
  ([c]
   `(let [v# (~'<! ~c)]
      (if (instance? js/Error v#) (throw v#) v#)))
  ([pred? ->message c]
   `(let [v# (~'<! ~c)]
      (cond (instance? js/Error v#) (throw v#)
            (~pred? v#) v#
            :else (throw (js/Error. (~->message v#)))))))

(defmacro alts? [cs]
  `(let [[e# :as v#] (~'alts! ~cs)]
     (if (instance? js/Error e#) (throw e#) v#)))

; TODO: update delay-chan* to current core.async
#_(defmacro delay-chan [& forms]
  `(xn.library.async/delay-chan* (delay ~@forms)))

