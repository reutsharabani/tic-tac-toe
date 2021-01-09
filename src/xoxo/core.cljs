(ns xoxo.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.dom :as rd]))

(enable-console-print!)

(defonce app-state (atom {:current-player "X"}))

(defn inc-handler [_]
  (swap! app-state update :count inc))

(defn taken-by [x y]
  (get-in @app-state [:board x y]))

(defn check-victory []
  (let [rows    (for [x (range 3)]
                  (into []
                        (for [y (range 3)]
                          [x y])))
        columns (for [y (range 3)]
                  (into []
                        (for [x (range 3)]
                          [x y])))
        cross-1 [[[0 0] [1 1] [2 2]]]
        cross-2 [[[0 2] [1 1] [2 0]]]]
    (doseq [streak (concat rows columns cross-1 cross-2)]
      (let [occupiers (frequencies (map #(apply taken-by %) streak))]
        (when (contains? #{{"X" 3} {"Y" 3}} occupiers)
          (swap! app-state assoc
                 :winning-streak (set streak)
                 :winner (-> occupiers keys first)))))))

(defn place [x y]
  (fn []
    (swap! app-state assoc-in [:board x y] (:current-player @app-state))
    (check-victory)
    (swap! app-state update :current-player (fn [cp]
                                              (if (= cp "X")
                                                "Y"
                                                "X")))
    ))

(defn winner? [s]
  (:winner s))

(defn cell [x y]
  [:td {:class         (str "board-cell"
                            (when (contains? (:winning-streak @app-state) [x y])
                              " winner-cell"))
        :border        "10px"
        :key           (str "cell " x y)
        :on-click      (when-not (or (taken-by x y)
                                     (winner? @app-state))
                         (place x y))
        :on-mouse-out  #(swap! app-state assoc :hovered-cell [])
        :on-mouse-over #(when-not (or (taken-by x y)
                                      (winner? @app-state))
                          (swap! app-state assoc :hovered-cell [x y]))}
   (let [alt-piece (when (= [x y] (:hovered-cell @app-state))
                     (:current-player @app-state))]
     (or (taken-by x y) alt-piece))])

(defn row [x]
  [:tr {:class "board-row"
        :key   (str "row-" x)}
   (doall (for [y (range 3)]
            (cell x y)))])

(defn winner []
  [:tr [:td
        {:colSpan "3"
         :class   "winner"} (str (winner? @app-state) " won")]])

(defn board []
  [:table
   [:tbody
    [:tr
     [:th {:on-click #(reset! app-state {:current-player "X"})
           :class    "reset-button"
           :colSpan  "3"} "reset"]]
    (doall
      (for [x (range 3)]
        (row x)))
    (if (winner? @app-state)
      (winner))]])

(defn xoxo []
  [:div
   (board)])

(rd/render [xoxo]
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
