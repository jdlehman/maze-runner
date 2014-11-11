(ns maze-runner.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn init-maze [row col]
  "Generates initial maze state (empty maze)"
  (into [] (repeat row
    (into [] (repeat col
                     {:north true :east true :south true :west true :visited false})))))

(def width 12)
(def height 12)

(defonce app-state
  (atom
    {:maze (init-maze width height) }))

(def directions
  [:north :east :south :west])

(defn get-cell [row col]
  (get-in @app-state [:maze row col]))

(defn cell-exists? [row col]
  (not (nil?
         (get-in @app-state
                 [:maze row col]))))

(defn has-wall? [direction row col]
  (direction (get-cell row col)))

(defn set-wall [direction row col val]
  (swap! app-state
         update-in
         [:maze row col]
         #(assoc % direction val)))

(defmulti destroy-wall
  (fn [direction row col]
    (set-wall direction row col false)
    direction))

(defmethod destroy-wall :north [direction row col]
  (let [south-row (- row 1)]
    (when (cell-exists? south-row col)
      (set-wall :south south-row col false))))

(defmethod destroy-wall :south [direction row col]
  (let [north-row (+ row 1)]
    (when (cell-exists? north-row col)
      (set-wall :north north-row col false))))

(defmethod destroy-wall :east [direction row col]
  (let [west-col (+ col 1)]
    (when (cell-exists? row west-col)
      (set-wall :west row west-col false))))

(defmethod destroy-wall :west [direction row col]
  (let [east-col (- col 1)]
    (when (cell-exists? row east-col)
      (set-wall :east row east-col false))))

(defn set-visited [row col]
  (swap! app-state
         update-in
         [:maze row col]
         #(assoc % :visited true)))

(defmulti move
  (fn [direction row col]
    (destroy-wall direction row col)
    (set-visited row col)
    direction))

(defmethod move :north [direction row col]
  [(- row 1) col])

(defmethod move :east [direction row col]
  [row (+ col 1)])

(defmethod move :south [direction row col]
  [(+ row 1) col])

(defmethod move :west [direction row col]
  [row (- col 1)])

(defn unvisited-cells? []
  (not (= (count (filter #(= (:visited %) false)
          (flatten (:maze @app-state))))
          0)))

(defn out-of-bounds? [row col]
  (or
    (< row 0) (< col 0)
    (>= row width) (>= col height)))

(defn is-visited? [row col]
  (or (out-of-bounds? row col)
      (get-in @app-state [:maze row col :visited])))

(defmulti neighbor-visited?
  (fn [direction & _]
    direction))

(defmethod neighbor-visited? :north [_ row col]
  (is-visited? (- row 1) col))

(defmethod neighbor-visited? :east [_ row col]
  (is-visited? row (+ col 1)))

(defmethod neighbor-visited? :south [_ row col]
  (is-visited? (+ row 1) col))

(defmethod neighbor-visited? :west [_ row col]
  (is-visited? row (- col 1)))

(defn unvisited-neighbors [row col]
  (remove #(neighbor-visited? % row col)
          directions))

(defn has-unvisited-neighbors [row col]
  (> (count (unvisited-neighbors row col))
     0))

(defn generate-maze [row col stack]
  (set-visited row col)
  (when (unvisited-cells?)
    (if (has-unvisited-neighbors row col)
      (let [dir (rand-nth (unvisited-neighbors row col))
            [new-row new-col] (move dir row col)
            new-stack (conj stack [row col])]
        (recur new-row new-col new-stack))
      (let [[old-row old-col] (last stack)
             old-stack (into [] (take (- (count stack) 1) stack))]
        (when (> (count old-stack) 0) ; is this needed?
          (recur old-row old-col old-stack))))))

(generate-maze 0 0 [])

(defn cell-style [{:keys [north east south west] :as cell}]
  (let [north-styles (if north " north " "")
        east-styles (if east " east " "")
        south-styles (if south " south " "")
        west-styles (if west " west " "")]
    (str north-styles east-styles south-styles west-styles)))

(defn maze-cell [cell owner]
  (om/component
    (dom/div #js {:className (str "cell " (cell-style cell))} " ")))

(defn maze-row [row owner]
  (om/component
    (apply dom/div #js {:className "row"}
           (om/build-all maze-cell row))))

(defn maze-view [app owner]
  (om/component
    (apply dom/div #js {:className "maze"}
           (om/build-all maze-row (:maze app)))))

(defn main []
  (om/root
    maze-view
    app-state
    {:target (. js/document (getElementById "app"))}))
