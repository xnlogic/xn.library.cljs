(ns xn.library.element
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [xn.core :refer [navigate]]))


(defn event-value
  ([e]
   (.. e -target -value))
  ([f e]
   (if f
     (f (event-value e))
     (event-value e))))

(defn prevent-default
  "Call with a fn as an argument and it will return a fn that will prevent default, then call the fn.
   Or call with an event and it will call preventDefault on the event.
   Or, called with an unrecognized argument and it will return a fn that will prevent default on events."
  [x]
  (cond
    (fn? x) (fn [e] (.preventDefault e) (x))
    (.-preventDefault x) (.preventDefault x)
    :else prevent-default))


(defn link [on-click text]
  (dom/a
    (condp apply [on-click]
      fn? #js {:href "#" :onClick (prevent-default on-click)}
      string? (if (= (subs on-click 0 1) "/")
                #js {:href "#" :onClick (prevent-default #(navigate on-click))}
                #js {:href on-click})
      vector? (prevent-default #(navigate on-click))
      prevent-default)
    text))

(defn danger [& body]
  (apply dom/div
         #js {:className "alert alert-danger"}
         body))

(defn info [& body]
  (apply dom/div
         #js {:className "alert alert-info"}
         body))

(defn icon
  ([icon_name]
    (icon nil icon_name))
  ([opts icon_name]
    (dom/i
      (clj->js (merge opts {:className (str "fa " (:className opts) " " icon_name)})))))

(defn modal
  ([opts body]
   (modal opts nil nil body))
  ([opts title body]
   (modal opts title nil body ))
  ([opts title footer & body]
   (do
     (dom/div #js {:className "modal modal-backdrop"})
     (dom/div #js {:className "modal show"}
              (dom/div #js {:className "modal-dialog"}
                       (dom/div #js {:className "modal-content"}
                                (when title
                                  (dom/div #js {:className "modal-header"}
                                           (dom/h4 #js {:className "modal-title"}
                                                    title
                                                    (icon {:onClick (:onClose opts) :className "pull-right "} "fa-close"))))
                                (apply dom/div #js {:className "modal-body"}
                                       body )
                                (when footer
                                  (dom/div #js {:className "modal-footer"} footer))))))))


(defn form [on-submit & body]
  (apply dom/form
         #js {:role "form"
              :onSubmit (prevent-default on-submit)}
         body))

(defn badge
  ([body]
    (badge [nil body]))
  ([opts body]
    (dom/span
      (clj->js (merge opts {:className (str "badge " (:className opts))}))
      body)))

(defn button
  ([body]
   (button nil nil body))
  ([f body]
   (button nil f body))
  ([opts f body]
   (dom/button
     (clj->js (merge opts {:className (str "btn btn-default " (:className opts))
                           :onClick f}))
     body)))

(defn label [label input]
  (dom/div
    #js {:className "form-group"}
    (dom/label
      nil label
      input)))

(defn list-item
  ([f body]
    (list-item nil f body))
  ([opts f body]
    (dom/li
      (clj->js (merge opts {:className (str "list-group-item " (:className opts))
                            :onClick f}))
    body)))

(defn select
  ([f body]
    (select nil f body))
  ([opts f body]
    (dom/select
      (clj->js (merge opts {:className (str "form-control " (:className opts))
                            :onClick f}))
    body)))

(defn row
  ([body]
    (row nil body))
  ([opts & body]
    (apply dom/div
      (clj->js (merge opts {:className (str "row-fluid " (:className opts))}))
      body)))

(defn page-heading [title & controls]
  (dom/div
    #js {}
    (dom/h4 #js {} title)
    (apply dom/div
           #js {}
           controls)))

(defn column [sizes & body]
  (let [cols (map (fn [[screen width]] (str "col-" (name screen) "-" width )) sizes)]
    (apply dom/div
      #js {:className (clojure.string/join " " cols)}
      body)))

(defn panel-body
  ([body]
   (panel-body nil body))
  ([opts & body]
   (apply dom/div
          (clj->js (merge opts {:className (str "panel-body")})))))

(defn panel
  ([body]
    (panel nil nil body))
  ([heading body]
    (panel nil heading body))
  ([opts heading & body]
    (dom/div
      (clj->js (merge opts {:className (str "panel panel-default " (:className opts)) :id (:id opts)}))
      (when heading
        (dom/div
          #js {:className "panel-heading"}
          (if (string? heading)
            (dom/h5 nil heading)
            heading)))
      (apply dom/div
        #js {:className "panel-body"}
        body))))

(defn table
  "Basic structure:
     head-rows and rows are both a vector of vectors of cell contents.

   A 'cell contents' can be either something renderable or a 2 element vector
   of React opts and something renderable [{:className etc} renderable].

   Any head rows or row can be annotated with React opts like ^{:className etc} [row ...]"
  ([rows] (table nil nil rows))
  ([head-rows rows] (table nil head-rows rows))
  ([opts head-rows rows]
   (letfn [(get-opts [x]
             (when (vector? x) (clj->js (first x))))
           (meta-opts [x]
             (when (meta x) (clj->js (meta x))))
           (renderable [x]
             (if (vector? x) (second x) x))]
     (dom/table
       opts
       (when (seq head-rows)
         (apply
           dom/thead nil
           (map (fn [head-row]
                  (apply
                    dom/tr (meta-opts head-row)
                    (map (fn [c] (dom/th (get-opts c) (renderable c)))
                         head-row)))
                head-rows)))
       (when (seq rows)
         (apply
           dom/tbody
           nil
           (map
             (fn [row]
               (apply
                 dom/tr (meta-opts row)
                 (map (fn [c] (dom/td (get-opts c) (renderable c)))
                      row)))
             rows)))))))

(defn accordion-row [opts & body]
  (apply dom/div #js {:className "row"}
         body))

(defn accordion
  "Defines an array of rows where the first row is clickable and expands
   a set of child rows.

   `open?` (open? row) -> true if the row's children should be visible
   `children` (children row) -> the row's children
   `child-element` (child-element child) -> a collection of divs
   `on-open` and `on-close` are higher-order functions that should return a click handler."
  [rows & {:keys [open? children
                  on-open on-close
                  header-element
                  can-remove? on-remove
                  child-header-element
                  empty-element
                  child-element]}]
  (apply dom/div
    nil
    (for [parent rows]
      (let [children (children parent)
            open? (and (open? parent) (pos? (count children)))
            on-click (if open? (on-close parent) (on-open parent))
            can-remove? (and can-remove? (can-remove? parent))]
        (dom/div
          #js {:className "panel panel-default application"}
          (dom/div
            #js {:className "panel-heading"}
            (dom/i #js {:className (str "fa fa-caret-" (if open? "down" "right") " toggle")
                        :onClick on-click})
            (cond->> (header-element parent)
              on-click (link on-click))
            (when can-remove?
              (dom/div #js {:className "pull-right"}
                (link (on-remove parent) (dom/i #js {:className "fa fa-remove"})))))
            (cond
              (and open? (pos? (count children)))
                (apply dom/div #js {:className "panel-body"}
                  (when child-header-element
                    (child-header-element))
                  (map child-element children))
              (= 0 (count children))
                (dom/div #js {:className "panel-body"}
                  (when empty-element (empty-element))
              )))))))
