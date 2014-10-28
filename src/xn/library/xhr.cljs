(ns ^{:doc "Make network requests."}
  xn.library.xhr
  (:require goog.net.XhrManager
            [cljs.core.async :as async :refer [<! >! chan alts! put!]]
            [cognitect.transit :as t])
  (:require-macros
    [cljs.core.async.macros :as m :refer [go]]))


(defn cancel [xhr]
  (when (.isActive (.-xhrIo xhr))
    (.abort (.-xhrIo xhr))))


; Build Request ----------------------------------------------------------------


(def method-map {:get "GET" :put "PUT" :post "POST" :patch "PATCH" :delete "DELETE"})

(def ^:private
  *xhr-manager*
  (goog.net.XhrManager. nil {} nil nil 5000))


(defn- handle-response [url start-time out interchange callback]
  (fn [event]
    (let [r (.-target event)
          data (case interchange
                 :json (try (js->clj (.getResponseJson r) :keywordize-keys true) (catch js/Error e nil))
                 :json-transit (try (t/read (t/reader :json) (.getResponse r)) (catch js/Error e nil))
                 :transit (try (t/read (t/reader :json) (.getResponse r)) (catch js/Error e nil)))
          status (.getStatus r)
          result (if data {:data data} {:body (.getResponse r)})
          result (merge result
                      {:success? (.isSuccess r)
                       :state (if (.isSuccess r) :complete :failed)
                       :url url
                       :status (if (zero? status) 500 status)
                       :status-text (if (zero? status) "Server Unavailable" (.getStatusText r))
                       :msecs (- (.getTime (js/Date.)) start-time)})]
      (when callback (callback nil (or data (.getResponse r))))
      (go (>! out result)))))


(def mime-type {:xml "application/xml"
                :json "application/json"
                :json-transit "application/transit+json"})


(defn request
  "Asynchronously make a network request for the resource at url.  The
   entry for `:event` contains an instance of the `goog.net.XhrManager.Event`.

   Other allowable keyword arguments are `:method`, `:content`, `:headers`,
   `:priority`, `:retries`, and `:timeout`. `:method` defaults to \"GET\" and `:retries`
   defaults to `0`. `:timeout` may be a number of milliseconds or a timeout channel.

   :cancel-on {some-chan \"Request cancelled because some-chan was closed.\"}
   The cancel-on argument is an optional map of channels to strings. If
   the channel is closed or written to will cause the request to be
   cancelled. The string will be used as the request cancellation reason.

   Returns a cancelable Request object with separate data and err channels"
  [url {:keys [method body headers priority retries id timeout out cancel-on
               xhr-chan interchange callback]
        :or   {method   :get
               out (chan)
               retries 0
               interchange :json}}]
  (let [start-time (js/Date.)
        headers (assoc headers :accept (mime-type interchange))]

    ; Currently does not specify an interchange format to the server
    (when-let [xhr (.send *xhr-manager*
                          (or id (js/Math.random))
                          url
                          (method-map method)
                          body
                          (into {} (map (fn [[k v]] [(name k) v]) headers))
                          priority
                          (handle-response url start-time out interchange callback)
                          retries)]
      (when xhr-chan (go (>! xhr-chan xhr)))
      (if (or timeout cancel-on)
        (let [out2 (chan)]
          (go
            (let [timeout (cond (number? timeout) [(async/timeout timeout)]
                                timeout [timeout])
                  [result c] (alts! (concat [out] timeout (keys cancel-on)))]
              (if (and (= out c) result)
                (>! out2 result)
                (do
                  (cancel xhr)
                  (let [err (if (= timeout c)
                              {:success? false
                               :state :failed
                               :url url
                               :status 408
                               :status-text "Request Timeout"
                               :msecs (- (js/Date.) start-time)}
                              {:success? false
                               :state :failed
                               :url url
                               :status (when-not (get cancel-on c) 500)
                               :status-text (str "Request Cancelled: "
                                                 (if-let [reason (get cancel-on c)]
                                                   reason
                                                   "Server Unavailable"))
                               :cancelled-by c
                               :msecs (- (js/Date.) start-time)})]
                    (when callback (callback (ex-info (:status-text err) err) nil))
                    (>! out2 err))))))
          out2)
        out))))

; Process Response

(defn success? [response]
  (:success? response))


(defn timeout? [response]
  (= "408" (:status response)))

