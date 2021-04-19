(ns organism.play
  (:require
   [clojure.string :as string]
   [cljs.pprint :refer (pprint)]
   [cljs.reader :as reader]
   [goog.events :as events]
   [goog.history.EventType :as HistoryEventType]
   [reitit.core :as reitit]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.base :as base]
   [organism.game :as game]
   [organism.choice :as choice]
   [organism.board :as board]
   [organism.dom :as dom]
   [organism.ajax :as ajax]
   [organism.websockets :as ws])
  (:import goog.History))

(def font-choice
  "BlinkMacSystemFont,-apple-system,\"Segoe UI\",Roboto,Oxygen,Ubuntu,Cantarell,\"Fira Sans\",\"Droid Sans\",\"Helvetica Neue\",Helvetica,Arial,sans-serif")

(defonce session (r/atom {:page :home}))
(defonce chat (r/atom []))

(defonce history-advance
  (r/atom nil))

(defonce player-order
  (r/atom
   (vec
    (take
     (count board/default-player-order)
     (repeat "")))))

;; (defonce player-order
;;   (r/atom board/default-player-order))

(defonce player-captures-order
  (r/atom
   (vec
    (repeat
     (count board/default-player-order)
     board/default-player-captures))))

(defonce board-invocation
  (r/atom (board/empty-invocation)))

(def empty-game-state
  {:game {}
   :created false
   :player js/playerKey
   :history []
   :cursor nil
   :board {}
   :turn :open
   :choices []})

(defonce game-state
  (r/atom empty-game-state))

(defonce clear-state
  (r/atom (game/initial-state board/default-player-order)))

(def empty-introduction
  {:chosen-space nil
   :chosen-element nil
   :progress {}})

(defonce introduction
  (r/atom empty-introduction))

(defonce food-source
  (r/atom {}))

(defonce player-games
  (r/atom {}))

(def max-players 10)

(def highlight-element-stroke {:ratio 0.04 :color "#ccc"})

(defn choose-food-source!
  [space]
  (swap! food-source update space inc))

(defn introduction-complete?
  [{:keys [progress]}]
  (and
   (= 3 (count (set (keys progress))))
   (= 3 (count (set (vals progress))))))

(defn assoc-prop
  [el key value]
  (if (> 1 (count el))
    (if (map? (nth el 1))
      (assoc-in el [1 key] value)
      (vec
       (concat
        [(first el) {key value}]
        (rest el))))
    (vec
     (conj
      (conj
       (rest el)
       {key value})
      (first el)))))

(defn send-state!
  [state complete]
  (ws/send-transit-message!
   {:type "game-state"
    :game state
    :complete complete}))

(defn send-reset!
  [state]
  (ws/send-transit-message!
   {:type "history"
    :game state}))

(defn send-clear!
  []
  (ws/send-transit-message!
   {:type "clear"}))

(defn send-choice!
  [choices match complete]
  (let [choice (get-in choices [match :state])]
    (if choice
      (send-state! choice complete)
      (println "NO CHOICE MATCHING" match (keys choices)))))

(defn send-introduction!
  [choices {:keys [progress] :as intro}]
  (if (introduction-complete? intro)
    (send-choice!
     choices
     (assoc progress :organism 0)
     true)))

(defn send-create!
  [invocation]
  (ws/send-transit-message!
   {:type "create"
    :invocation invocation}))

(defn send-open-game!
  [invocation]
  (ws/send-transit-message!
   {:type "open-game"
    :invocation invocation}))

(defn initialize-chat
  [chat message]
  (println "initializing chat" (:chat message))
  (:chat message))

(defn initialize-game
  [game-state {:keys [game invocation player history board] :as message}]
  (let [{:keys [ring-count player-count players colors]} invocation
        board (board/generate-board
               colors
               players
               (take ring-count board/total-rings))
        [turn choices] (choice/find-state game)]
    (println "initializing game" game)
    (println "initializing board" board)
    (println "turn" turn)
    (println "choices" (count choices))
    {:game game
     :invocation invocation
     :player player
     :history history
     :board board
     :turn turn
     :choices choices}))

(defn update-chat
  [chat message]
  (conj chat message))

(defn update-game
  [game-state message]
  (let [state (:game message)
        current-game (assoc (:game game-state) :state state)
        [final-game turn choices] (choice/find-next-choices current-game)
        game-state (assoc game-state :game final-game)]
    (-> game-state
        (update :history conj (:state final-game))
        (assoc :turn turn)
        (assoc :choices choices))))

(def number->word
  {3 "THREE"
   4 "FOUR"
   5 "FIVE"
   6 "SIX"
   7 "SEVEN"
   8 "EIGHT"})

(defn round-banner
  [color round]
  [:div
   {:style
    {:color "#fff"
     :border-radius "50px"
     :cursor "pointer"
     :background color
     :letter-spacing "8px"
     :font-family font-choice
     :margin "20px 0px"
     :padding "25px 60px"}}
   [:h1
    [:a
     {:style
      {:color "#fff"}
      :href
      (if js/playerKey
        (str "/player/" js/playerKey)
        "/")}
     js/gameKey]]
   [:h2 "round " round]])

(defn boundary-inc
  [total n]
  (cond
    (nil? n) nil
    (= n (dec total)) nil
    :else (inc n)))

(defn boundary-dec
  [total n]
  (cond
    (nil? n) (dec total)
    (zero? n) 0
    :else (dec n)))

(defn history-beginning-control
  []
  [:polygon
   {:points "0,5 10,5 10,25 50,0 50,50 10,25 10,45 0,45"
    :style {:fill "hsl(100, 20%, 20%)"}
    :on-click
    (fn [event]
      (swap! game-state assoc :cursor 0))}])

(defn history-back-control
  [cursor total]
  [:polygon
   {:points "70,25 100,5 100,45"
    :style {:fill "hsl(100, 20%, 20%)"}
    :on-click
    (fn [event]
      (swap! game-state update :cursor (partial boundary-dec total)))}])

(defn clear-history-advance!
  [advance]
  (.clearInterval
   js/window
   advance)
  (reset! history-advance nil))

(defn history-status-display
  [cursor total]
  [:text
   {:x (if cursor "110" "140")
    :y "35"
    :width "80"
    :font-size "1.5em"
    :on-click
    (fn [event]
      (if-let [advance @history-advance]
        (clear-history-advance! advance)
        (reset!
         history-advance
         (.setInterval
          js/window
          (fn []
            (if (= cursor (dec total))
              (clear-history-advance! @history-advance)
              (swap! game-state update :cursor (partial boundary-inc total))))
          300))))}
   (if cursor
     (str (inc cursor) " / " total)
     total)])

(defn history-forward-control
  [total]
  [:polygon
   {:points "200,5 230,25 200,45"
    :style {:fill "hsl(100, 20%, 20%)"}
    :on-click
    (fn [event]
      (swap! game-state update :cursor (partial boundary-inc total)))}])

(defn history-end-control
  [total]
  [:polygon
   {:points "300,5 290,5 290,25 250,0 250,50 290,25 290,45 300,45"
    :style {:fill "hsl(100, 20%, 20%)"}
    :on-click
    (fn [event]
      (swap! game-state assoc :cursor nil))}])

(defn history-controls
  [history cursor]
  (let [total (count history)]
    [:svg
     {:width 300
      :height 50
      :style
      {:margin "40px 0px 0px 0px"}}
     [:g
      {:transform "scale(0.6)"}
      [history-beginning-control]
      [history-back-control cursor total]
      [history-status-display cursor total]
      [history-forward-control total]
      [history-end-control total]]]))

(defn scoreboard
  [turn-order organism-victory colors player-captures state]
  [:div
   [:h3 "score"]
   [:ul
    (let [player-captures (if player-captures player-captures (repeat board/default-player-captures))]
      (for [[player captures color] (map vector turn-order player-captures colors)]
        ^{:key (str player color)}
        [:li
         {:style {:color color}}
         player " - "
         (count (get-in state [:captures player])) " / "
         captures]))]
   [:h4
    {:style
     {:font-size "1.0em"
      :margin "12px 0px 0px 0px"}}
    (let [player (get-in state [:player-turn :player])
          player-colors (into {} (map vector turn-order colors))]
      [:span
       {:style
        {:color (get player-colors player)}}
       (get number->word organism-victory organism-victory)])
    " organisms for victory"]])

(def chat-window 15)

(defn chat-list
  [player-colors chat]
  [:ul
   (let [total (count chat)
         visible (drop (- total chat-window) chat)]
     (for [[i message] (map-indexed vector visible)]
       (let [player (:player message)
             color (get player-colors player "black")]
         ^{:key i}
         [:li
          {:style {:color color}}
          player ": " (:message message)])))])

(defn chat-input
  []
  (if js/playerKey
    (let [value (r/atom nil)]
      (fn []
        [:input.form-control
         {:type :text
          :placeholder "respond"
          :value @value
          :on-change #(reset! value (-> % .-target .-value))
          :on-key-down
          #(when (= (.-keyCode %) 13)
             (ws/send-transit-message!
              {:type "chat"
               :player js/playerKey
               :message @value})
             (reset! value nil))}]))
    []))

(defn description-panel
  [player-color description]
  [:div
   [:h4
    {:style
     {:color player-color}}
    description]])

(defn chat-panel
  [description turn-order organism-victory colors player-colors player-captures
   state history cursor chat]
  (let [player-color (get player-colors (-> state :player-turn :player) (first colors))]
    [:div
     {:style
      {:margin "20px"}}
     [round-banner
      player-color
      (:round state)]
     [:div
      {:style
       {:margin "20px 50px"}}
      [description-panel player-color description]
      [scoreboard turn-order organism-victory colors player-captures state]
      [history-controls history cursor]
      [:br]
      [:h3 "discussion"]
      [chat-list player-colors chat]
      [:br]
      [chat-input]]]))

(defn highlight-circle
  [x y radius color on-click]
  (let [highlight-color (board/brighten color 0.3)]
    [:circle
     {:cx x :cy y
      :r (* radius 1.1)
      :stroke highlight-color
      :stroke-width (* 0.19 radius)
      :fill-opacity 0.1
      :fill "white"
      :on-click on-click}]))

(defn focus-circle
  [x y radius color on-click]
  [:circle
   {:cx x :cy y
    :r radius
    :stroke (board/brighten color 0.2)
    :stroke-width (* 0.21 radius)
    :fill (board/brighten color 0.1)
    :on-click on-click}])

(defn highlight-element
  [type food x y radius color stroke on-click]
  (let [g (board/render-element
           (board/brighten color 0.1)
           "white"
           stroke
           [x y]
           radius
           {:type type :food food})]
    (assoc-prop g :on-click on-click)))

(defn render-element
  [type x y radius color food-color on-click]
  (let [g (board/render-element
           color
           food-color
           [x y]
           radius
           {:type type :food 1})]
    (assoc-prop g :on-click on-click)))

(def highlight-factor 0.93)
(def element-highlight-factor 1.0)

(defn create-highlights
  [game board colors turn choices]
  (let [players (:players game)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        highlights
        (base/map-cat
         (fn [[[player {:keys [starting-spaces]}] color]]
           (map
            (fn [space]
              (let [[x y] (get locations space)]
                (highlight-circle
                 x y radius color
                 (fn [event]))))
            starting-spaces))
         (map vector players colors))]
    (into [] (concat [:g] highlights))))

(defn introduce-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        food-color (-> board :colors first last)
        locations (:locations board)
        radius (* (:radius board) highlight-factor)
        element-radius (* (:radius board) 1)
        starting-spaces (get-in game [:players player :starting-spaces])
        {:keys [chosen-space chosen-element progress]} @introduction

        ;; unchosen starting spaces
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-circle
              x y radius color
              (fn [event]
                (if chosen-element
                  (do
                    (swap!
                     introduction
                     (fn [intro]
                       (-> intro
                           (assoc :chosen-space nil)
                           (assoc :chosen-element nil)
                           (update :progress assoc chosen-element space))))
                    (send-introduction! choices @introduction))
                  (swap! introduction assoc :chosen-space space))))))
         (remove
          (set (conj (vals progress) chosen-space))
          starting-spaces))

        ;; chosen space without element
        highlights
        (if chosen-space
          (let [[x y] (get locations chosen-space)]
            (conj
             highlights
             ^{:key chosen-space}
             (focus-circle
              x y radius color
              (fn [event]
                (swap! introduction dissoc :chosen-space)))))
          highlights)

        ;; elements placed so far
        elements
        (map
         (fn [[type space]]
           (let [[x y] (get locations space)]
             ^{:key space}
             (render-element
              type x y element-radius color food-color
              (fn [event]
                (if chosen-element
                  (do
                    (swap!
                     introduction
                     (fn [intro]
                       (-> intro
                           (assoc :chosen-space nil)
                           (assoc :chosen-element nil)
                           (update :progress assoc chosen-element space))))
                    (send-introduction! choices @introduction))
                  (swap! introduction assoc :chosen-space space))))))
         progress)]
    ^{:key "highlights"}
    [:g (concat highlights elements)]))

(defn chosen-organism-highlights
  [game board on-click turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) element-highlight-factor)
        elements (game/current-organism-elements game)
        element-stroke highlight-element-stroke
        source @food-source

        highlights
        (mapv
         (fn [{:keys [space organism type food] :as element}]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-element
              type (- food (get source space 0))
              x y radius
              color element-stroke
              (partial on-click element))))
         elements)]
    highlights))

(defn space-highlights
  [game board turn choices spaces on-click]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)

        space-highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)
                 next-state (get-in choices [space :state])]
             ^{:key space}
             (highlight-circle
              x y radius color
              (partial on-click space))))
         spaces)]
    space-highlights))

(defn choose-organism-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        organisms (game/player-organisms game player)
        available (keys choices)
        elements (base/map-cat organisms available)
        spaces (map :space elements)
        space-organisms
        (into
         {}
         (map
          (juxt :space :organism)
          elements))

        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (let [organism (get space-organisms space)]
             (send-choice! choices organism true))))]
    
    highlights))

(defn choose-action-type-highlights
  [game board turn choices]
  (chosen-organism-highlights
   game board 
   (fn [element event]
     (send-choice! choices (:type element) true))
   turn choices))

(defn choose-space-highlights
  [game board turn choices]
  (let [spaces (keys choices)
        elements (game/current-organism-elements game)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [element event]
           (if (get choices (:space element))
             (send-choice! choices (:space element) true)))
         turn choices)
        
        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (send-choice! choices space true)))]
    (concat highlights element-highlights)))

(defn choose-target-highlights
  [game board turn choices]
  (let [choose-from (game/get-action-field game :from)
        spaces (keys choices)
        element-stroke highlight-element-stroke

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [element event]
           (if (get choices (:space element))
             (send-choice! choices (:space element) true)))
         turn choices)
        
        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (send-choice! choices space true)))]

    (concat highlights element-highlights)))

(defn grow-element-highlights
  [game board turn choices]
  (chosen-organism-highlights
   game board
   (fn [element event]
     (send-choice! choices (:type element) true))
   turn choices))

(defn grow-from-highlights
  [game board turn choices]
  (let [player (game/current-player game)
        color (get-in board [:player-colors player])
        locations (:locations board)
        radius (* (:radius board) highlight-factor)

        source @food-source
        elements (game/current-organism-elements game)
        fed-growers (filter
                     (fn [{:keys [type space food]}]
                       (and
                        (= :grow type)
                        (< 0 (- food (get source space 0)))))
                     elements)
        spaces (map :space fed-growers)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [{:keys [space] :as element} event]
           (when ((set spaces) space)
             (choose-food-source! space)
             (let [source @food-source]
               (if (get choices source)
                 (send-choice! choices source true)))))
         turn choices)
        
        highlights
        (mapv
         (fn [space]
           (let [[x y] (get locations space)]
             ^{:key space}
             (highlight-circle
              x y radius (board/brighten color 0.2)
              (fn [event]
                (choose-food-source! space)
                (let [source @food-source]
                  (if (get choices source)
                    (send-choice! choices source true)))))))
         spaces)]
    (concat highlights element-highlights)))

(defn grow-to-highlights
  [game board turn choices]
  (let [spaces (keys choices)
        elements (game/current-organism-elements game)

        element-highlights
        (chosen-organism-highlights
         game board 
         (fn [element event]
           (if (get choices (:space element))
             (send-choice! choices (:space element) true)))
         turn choices)
        
        highlights
        (space-highlights
         game board turn choices
         spaces
         (fn [space event]
           (send-choice! choices space true)))]
    (concat highlights element-highlights)))

(defn find-highlights
  [game board colors turn choices]
  (let [highlights
        (condp = turn
          :open []
          :create (create-highlights game board colors turn choices)
          :introduce (introduce-highlights game board turn choices)
          :choose-organism (choose-organism-highlights game board turn choices)
          :choose-action-type (choose-action-type-highlights game board turn choices)
          :choose-action (choose-action-type-highlights game board turn choices)
          :eat-to (choose-space-highlights game board turn choices)
          :circulate-from (choose-space-highlights game board turn choices)
          :circulate-to (choose-target-highlights game board turn choices)
          :grow-element (choose-action-type-highlights game board turn choices)
          :grow-from (grow-from-highlights game board turn choices)
          :grow-to (choose-space-highlights game board turn choices)
          :move-from (choose-space-highlights game board turn choices)
          :move-to (choose-target-highlights game board turn choices)
          [])]
    ^{:key "highlights"}
    (if (empty? highlights)
      []
      (into [] (concat [:g] highlights)))))

(defn organism-board
  [game board colors turn choices]
  (println "organism board" colors turn choices board game)
  (let [svg (board/render-game board game)
        highlights (find-highlights game board colors turn choices)]
    (if (empty? highlights)
      svg
      (conj svg highlights))))

(defn generate-game-state
  [{:keys [ring-count player-count players colors player-captures] :as invocation}]
  (let [symmetry (board/player-symmetry player-count)
        rings (take ring-count board/total-rings)
        starting (board/find-starting-spaces symmetry rings players)
        game-players (game/initial-players starting player-captures)
        game {:players game-players}
        board
        (board/generate-board
         colors
         players
         rings)]
    (println "game players" game-players)
    {:game game
     :player js/playerKey
     :history []
     :board board
     :turn :create
     :choices []}))

(defn apply-invocation!
  [invocation]
  (println "INVOCATION" invocation)
  (let [generated (generate-game-state invocation)]
    (swap!
     player-captures-order
     (fn [captures-order]
       (reduce
        (fn [order [index captures]]
          (assoc order index captures))
        captures-order
        (map vector (range) (:player-captures invocation)))))
    (swap!
     player-order
     (fn [order]
       (reduce
        (fn [order [index player]]
          (assoc order index player))
        order
        (map vector (range) (:players invocation)))))
    (reset!
     game-state
     generated)))

(defn current-player-banner
  [player color turn]
  [:div
   {:style
    {:color "#fff"
     :border-radius "50px"
     :cursor "pointer"
     :background color
     :letter-spacing "8px"
     :font-family font-choice
     :margin "20px 0px"
     :padding "25px 60px"}}
   [:h1
    [:a
     {:style
      {:color "#fff"}
      :href
      (if js/playerKey
        (str "/player/" js/playerKey)
        "/")}
     player]]
   [:div
    {:style
     {:font-size "1.3em"
      :letter-spacing "5px"
      :margin "10px 0px"}}
    (string/join " " (string/split (name turn) #"-"))]])

(def turn-descriptions
  {:pass "pass"
   :actions-complete "resolve conflicts"
   :resolve-conflicts "check integrity"
   :player-victory "declare victory!"
   :check-integrity "confirm turn"})

(defn progress-control
  [turn choices advance]
  (if-let [description (get turn-descriptions turn)]
    [:span
     {:style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "20px"
       :background "hsl(100,50%,50%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "10px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (send-state! (get-in choices [advance :state]) true))}
     description]))

(defn current-action-index
  [num-actions actions]
  (cond
    (empty? actions) 0

    (game/complete-action? (last actions))
    (when (not= (count actions) num-actions)
      (count actions))

    :else (dec (count actions))))

(defn eat-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background "#fff"
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "eat"]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn grow-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    (println "GROW ACTION CONTROL" action (keys choices))
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background "#fff"
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :cursor "pointer"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "grow"]
     (if-let [element (-> action :action :element)]
       [:span
        {:style
         {:margin "0px 20px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "5px"
          :background "#fff"
          :font-size "1.0em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "2px 10px"}}
        element]
       [:span
        {:style
         {:margin "0px 10px"}}
        (map-indexed
         (fn [index element-choice]
           ^{:key element-choice}
           [:span
            [:span
             {:style
              {:margin "0px 10px"
               :color "#fff"
               :border-width "2px"
               :border-radius "5px"
               :background color
               :font-size "1.0em"
               :letter-spacing "7px"
               :font-family font-choice
               :cursor "pointer"
               :padding "2px 10px"}
              :on-click
              (fn [event]
                (if-let [choice (get choices element-choice)]
                  (send-state! (:state choice) true)))}
             element-choice]
            (if (not= index (dec (count choices)))
              " / ")])
         (keys choices))])
     [:span
      (if-let [from (-> action :action :from)]
        (concat
         (list " from ")
         (map
          (fn [[[ring space] food]]
            ^{:key [ring space]}
            [:span
             {:style
              {:background (get board-colors ring)
               :color "#fff"
               :margin "0px 5px"
               :font-family font-choice
               :padding "5px 5px"
               :border-radius "5px"}}
             (str " " ring " " space " : " food " ")])
          from)))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn move-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background "#fff"
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      "move"]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [from (-> action :action :from)]
        (list
         " from "
         [:span
          {:style
           {:background (get board-colors (first from))
            :color "#fff"
            :font-family font-choice
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " from)]))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn circulate-action-control
  [board-colors turn choices color action action-index]
  (let [complete? (game/complete-action? action)]
    [:div
     {:style
      {:margin "20px 0px"}}
     [:span
      {:style
       (if complete?
         {:margin "0px 5px"
          :color color
          :border-style "solid"
          :border-width "2px"
          :border-radius "10px"
          :background "#fff"
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"}
         {:margin "0px 5px"
          :color "#fff"
          :cursor "pointer"
          :border-width "2px"
          :border-radius "15px"
          :background color
          :font-size "1.2em"
          :letter-spacing "7px"
          :font-family font-choice
          :padding "5px 20px"})}
      (if (-> action :action :pass)
        "pass"
        "circulate")]
     [:span
      {:style
       {:margin "0px 5px"}}
      (if-let [from (-> action :action :from)]
        (list
         " from "
         [:span
          {:style
           {:background (get board-colors (first from))
            :color "#fff"
            :font-family font-choice
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " from)]))
      (if-let [to (-> action :action :to)]
        (list
         " to "
         [:span
          {:style
           {:background (get board-colors (first to))
            :color "#fff"
            :font-family font-choice
            :margin "0px 5px"
            :padding "5px 5px"
            :border-radius "5px"}}
          (string/join " " to)]))]]))

(defn pass-action-control
  [board-colors turn choices color action action-index])

(def action-control-map
  {:eat eat-action-control
   :grow grow-action-control
   :move move-action-control
   :circulate circulate-action-control
   :pass pass-action-control})

(defn past-action-control
  [board-colors turn choices color choice action action-index]
  [:div
   [(get action-control-map (:type action)) board-colors turn choices color action action-index]])

(defn choose-action-control
  [turn choices color choice]
  [:span
   {:style
    {:color "#fff"
     :border-radius "20px"
     :margin "20px 5px"
     :cursor "pointer"
     :background color
     :font-size "1.2em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}
    :on-click
    (condp = turn
      :choose-action
      (fn [event]
        (if (get choices choice)
          (send-choice! choices choice true)))
      (fn [event]))}
   choice])

(defn circulate-control
  [turn choices color]
  [:span
   {:style
    {:color "#fff"
     :border-radius "20px"
     :margin "20px 5px"
     :cursor "pointer"
     :background color
     :font-size "1.2em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}
    :on-click
    (condp = turn
      :choose-action
      (fn [event]
        (if (:circulate choices)
          (send-choice! choices :circulate true)))
      (fn [event]))}
   "circulate"])

(defn current-action-control
  [board-colors turn choices color choice action action-index]
  (if-let [type (:type action)]
    [:div
     [(get action-control-map type) board-colors turn choices color action action-index]]
    [:div
     {:style
      {:margin "20px 0px"}}
     (if (get choices choice)
       [choose-action-control turn choices color choice])
     (if (:circulate choices)
       [:span
        " / "
        [circulate-control turn choices color]])]))

(defn future-control
  [color choice]
  [:span
   {:style
    {:color color
     :border-style "solid"
     :border-width "2px"
     :border-radius "10px"
     :margin "20px 5px"
     :background "#fff"
     :font-size "1.0em"
     :letter-spacing "7px"
     :font-family font-choice
     :padding "5px 20px"}}
   choice])

(defn future-action-control
  [board-colors turn choices color choice action action-index]
  [:div
   {:style
    {:margin "20px 0px"}}
   [future-control color choice]
   " / "
   [future-control color "circulate"]])

(defn action-controls
  [board-colors turn choices color {:keys [choice num-actions actions] :as organism-turn}]
  (if choice
    (let [current-action (current-action-index num-actions actions)]
      (println "action controls" current-action turn (count choices) color organism-turn)
      [:div
       (map
        (fn [action-index]
          (let [action
                (if (< action-index (count actions))
                  (nth actions action-index)
                  {})]
            ^{:key action-index}
            (cond

              (nil? current-action)
              [past-action-control board-colors turn choices color choice action action-index]

              (> action-index current-action)
              [future-action-control board-colors turn choices color choice action action-index]

              (= action-index current-action)
              [current-action-control board-colors turn choices color choice action action-index]

              :else
              [past-action-control board-colors turn choices color choice action action-index])))

        (range num-actions))])
    [:div]))

(defn undo-control
  [turn choices state]
  [:div
   {:style
    {:font-family font-choice
     :margin "40px 0px"}}

   [:div
    {:style
     {:margin "15px 0px"}}
    [:span
     {:title "reset to the beginning of your turn"
      :style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "10px"
       :background "hsl(200,50%,80%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "0px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (if (and
             (= turn :introduce)
             (not= @introduction empty-introduction))
          (reset! introduction empty-introduction)
          (send-clear!)))}
     "clear"]]

   [:div
    {:style
     {:margin "15px 0px"}}
    [:span
     {:title "take one step back, potentially to previous player's turn"
      :style
      {:color "#fff"
       :cursor "pointer"
       :border-radius "10px"
       :background "hsl(0,50%,50%)"
       :font-size "1.2em"
       :letter-spacing "4px"
       :margin "0px 10px"
       :padding "5px 20px"}
      :on-click
      (fn [event]
        (if (and
             (= turn :introduce)
             (not= @introduction empty-introduction))
          (reset! introduction empty-introduction)
          (do
            (reset! food-source {})
            (send-reset! state))))}
     "undo"]]

   (when (= turn :choose-action)
     [:div
      {:style
       {:margin "15px 0px"}}
      [:span
       {:title "pass this action"
        :style
        {:color "#fff"
         :cursor "pointer"
         :border-radius "10px"
         :background "hsl(100,50%,50%)"
         :font-size "1.2em"
         :letter-spacing "4px"
         :margin "0px 10px"
         :padding "5px 20px"}
        :on-click
        (fn [event]
          (send-state!
           (-> {:state state}
               (game/choose-action :circulate)
               (game/pass-action)
               :state)
           true))}
       "pass"]])

   [:div
    {:style
     {:margin "15px 0px"}}
    [progress-control turn choices (if (= turn :pass) :pass :advance)]]])

(defn organism-controls
  [game board turn choices history]
  (let [player-turn (game/get-player-turn game)
        organism-turn (game/get-organism-turn game)
        action-type (:choice organism-turn)
        current-action (last (:actions organism-turn))

        player-colors (:player-colors board)
        current-player (game/current-player game)
        current-color (get player-colors current-player)
        dormant-color (board/brighten current-color -0.7)
        focus-color (board/brighten current-color 0.4)
        board-colors (into {} (:colors board))

        element-radius 45
        element-controls
        (map
         vector
         [[50 50] [150 50] [100 130]]
         [:eat :grow :move])
        {:keys [chosen-space chosen-element progress] :as introduce} @introduction]

    (if current-player
      [:div
       {:style
        {:margin "20px 20px"}}
       [current-player-banner current-player current-color turn]
       [:div
        {:style
         {:margin "0px 40px"}}
        [:svg
         {:width 200 :height 180}

         ;; ELEMENT CONTROLS
         (vec
          (concat
           [:g]
           (for [[location type] element-controls]
             (let [type->location
                   (into
                    {}
                    (map
                     (fn [[location type]]
                       [type location])
                     element-controls))

                   element-state
                   (cond 
                     (and
                      (not (= turn :choose-organism))
                      (or
                       (and
                        (= turn :introduce)
                        (= chosen-element type))
                       (= type action-type)))
                     :focus

                     (or
                      (and
                       (= turn :introduce)
                       (get progress type))
                      (not (nil? action-type)))
                     :dormant
                     :else :neutral)
                   
                   color
                   (condp = element-state
                     :focus focus-color
                     :dormant dormant-color
                     :neutral current-color)]

               ^{:key type}
               (-> (board/render-element
                    color color
                    {:ratio 0.02 :color "#ccc"}
                    location
                    element-radius
                    {:type type :food 0})
                   (assoc-prop :style {:cursor "pointer"})
                   (assoc-prop :title type)
                   
                   (assoc-prop
                    :on-click
                    (fn [event]
                      (condp = turn
                        :introduce
                        (if (= type chosen-element)
                          (swap!
                           introduction
                           dissoc
                           :chosen-element)
                          (if chosen-space
                            (do
                              (swap!
                               introduction
                               (fn [intro]
                                 (-> intro
                                     (dissoc :chosen-element)
                                     (dissoc :chosen-space)
                                     (update :progress (fn [pro] (assoc pro type chosen-space))))))
                              (send-introduction! choices @introduction))
                            (swap! introduction assoc :chosen-element type)))
                        :choose-action-type
                        (send-choice! choices type true)))))))))]

        [:br]

        (when-not (= turn :choose-organism)
          [action-controls board-colors turn choices current-color organism-turn])

        (if-not (-> game :state :winner)
          [undo-control turn choices (:state game)])]])))

(defn flex-direction
  [direction]
  {:style
   {:display "flex"
    :flex-direction flex-direction}})

(defn flex-grow
  [direction grow]
  (assoc-in
   (flex-direction direction)
   [:style :flex-grow]
   grow))

(defn game-layout
  [inner]
  [:div
   (flex-direction "column")
   ;; (assoc-in [:style :overflow-x] "scroll")
   inner])

(defn ring-count-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "ring-count"
       :name "ring-count"
       :value (:ring-count invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               invocation @board-invocation
               colors (board/generate-colors-buffer
                       board/total-rings
                       value
                       max-players)]
           (-> invocation
               (assoc :ring-count value)
               (assoc :colors colors)
               send-create!)))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 3 12))]
     [:label
      {:for "ring-count"
       :style
       {:font-size "1.5em"}}
      "rings"]]))

(defn player-count-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "player-count"
       :name "player-count"
       :value (:player-count invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)
               order @player-order
               captures-order @player-captures-order
               colors (board/generate-colors-buffer
                       board/total-rings
                       (:ring-count invocation)
                       max-players)
               players (vec (take value order))
               captures (vec (take value captures-order))]
           (-> invocation
               (assoc :colors colors)
               (assoc :player-count value)
               (assoc :players players)
               (assoc :player-captures captures)
               send-create!)))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 1 11))]
     [:label
      {:for "player-count"
       :style
       {:font-size "1.5em"}}
      "players"]]))

(defn organism-victory-input
  [color]
  (let [invocation @board-invocation]
    [:div
     [:select
      {:id "organism-victory"
       :name "organism-victory"
       :value (:organism-victory invocation)
       :style
       {:background-color color}
       :on-change
       (fn [event]
         (let [value (-> event .-target .-value js/parseInt)]
           (-> invocation
               (assoc :organism-victory value)
               (send-create!))))}
      (map
       (fn [n]
         ^{:key n}
         [:option
          {:value n}
          n])
       (range 3 14))]
     [:label
      {:for "organism-victory"
       :style
       {:font-size "1.5em"}}
      "organisms for victory"]]))

(defn send-player-name!
  [index player-name]
  (ws/send-transit-message!
   {:type "player-name"
    :index index
    :player player-name}))

(defn players-input
  [page-player invocation]
  (let [{:keys [player-count colors player-captures]} invocation
        order @player-order
        captures-order @player-captures-order]
    [:div
     [:h3
      {:style
       {:margin "20px 0px 0px 0px"}}
      [:span
       {:title "click an empty field to join the game\nor modify to add other players"}
       "players joined "]
      [:span
       {:title "how many captures each player is required to win"
        :style {:font-size "0.8em"}}
       " (capture limit)"]]
     (map
      (fn [index color player captures]
        ^{:key index}

        [:div
         [:input
          {:value player
           :style
           {:border-radius "25px"
            :color "#fff"
            :background color
            :border "3px solid"
            :font-size "1.5em"
            :letter-spacing "6px"
            :margin "2px 0px"
            :width "366px"
            :padding "10px 30px"}
           :on-focus
           (fn [event]
             (when (empty? player)
               (send-player-name! index page-player)
               (send-open-game!
                (update invocation :players assoc index page-player))))
           :on-blur
           (fn [event]
             (send-open-game! invocation))
           :on-change
           (fn [event]
             (let [value (-> event .-target .-value)]
               (send-player-name! index value)))}]

         [:select
          {:value captures
           :style
           {:background-color color}
           :on-change
           (fn [event]
             (let [value (-> event .-target .-value js/parseInt)]
               (swap!
                player-captures-order
                assoc index value)
               (-> invocation
                   (assoc
                    :player-captures
                    (vec
                     (take
                      player-count
                      @player-captures-order)))
                   (send-create!))))}

          (map
           (fn [n]
             ^{:key n}
             [:option
              {:value n}
              n])
           (range 1 14))]])
      (range)
      (reverse (take player-count (map last colors)))
      order
      player-captures)]))

(defn create-button
  [active-color inactive-color invocation]
  (let [valid? (board/valid-invocation? invocation)]
    [:input
     {:type :button
      :value (if valid? "CREATE" "incomplete")
      :style
      {:border-radius (if valid? "50px" "10px")
       :color "#fff"
       :cursor "pointer"
       :background (if valid? active-color inactive-color)
       :border "3px solid"
       :font-size "2em"
       :letter-spacing "8px"
       :margin "20px 40px"
       :padding "25px 60px"}
      :on-click
      (fn [event]
        (if valid?
          (ws/send-transit-message!
           {:type "trigger-creation"})
          (dom/redirect!
           (str "/player/" js/playerKey))))}]))

(defn description-input
  [{:keys [description] :as invocation} foreground-color background-color]
  [:div
   [:h3
    {:style
     {:margin "20px 0px 0px 0px"}}
    [:span
     {:title "explain a bit about the game you are creating for potential players"}
     "description"]]
   [:textarea
    {:value (or description "")
     :rows (inc (quot (count description) 49))
     :style
     {:border-radius "25px"
      :color foreground-color
      :background background-color
      ;; :border "3px solid"
      :font-size "0.9em"
      :letter-spacing "1px"
      :margin "2px 0px"
      :width "460px"
      :padding "10px 30px"}
     ;; :on-blur
     ;; (fn [event]
     ;;   (send-open-game!
     ;;    (assoc invocation :description @description)))
     :on-change
     (fn [event]
       (let [value (-> event .-target .-value)]
         (send-create!
          (assoc invocation :description value))))}]])

(defn invocation-player-colors
  [number invocation]
  (reverse
   (take
    number
    (map
     last
     (:colors invocation)))))

(defn create-page
  []
  (let [invocation @board-invocation
        {:keys [game board turn choices]} @game-state
        {:keys [state turn-order]} game
        turn-order (:players invocation)
        player-captures (:player-captures invocation)
        organism-victory (:organism-victory invocation)
        description (:description invocation)
        invocation-colors (invocation-player-colors (count turn-order) invocation)
        player-colors (into {} (map vector turn-order invocation-colors))
        create-color (-> invocation :colors rest first last)
        select-color (-> invocation :colors first last)
        inactive-color (-> invocation :colors last last)]
    (game-layout
     [:main
      (flex-grow "row" 1)
      [:nav
       {:style
        {:width "30%"}}
       [:div
        {:style
         {:margin "20px 20px"}}
        [current-player-banner js/playerKey (get player-colors js/playerKey inactive-color) "create game"]
        [create-button create-color inactive-color invocation]]
       [:form
        {:style
         {:margin "40px 60px"}}
        [ring-count-input select-color]
        [player-count-input select-color]
        [organism-victory-input select-color]
        [description-input invocation select-color inactive-color]
        [players-input js/playerKey invocation]]]
      [:article
       {:style {:flex-grow 1}}
       [organism-board game board invocation-colors turn choices]]
      (println "INVOCATION" invocation)
      [:aside
       {:style
        {:width "30%"}}
       [chat-panel description turn-order organism-victory invocation-colors player-colors player-captures state [] nil @chat]]])))

(defn game-page
  []
  (let [invocation @board-invocation
        {:keys [game board turn choices history cursor]} @game-state
        {:keys [state turn-order]} game
        {:keys [player-captures organism-victory description]} invocation
        state (if cursor (nth history cursor) state)
        game (assoc game :state state)
        invocation-colors (invocation-player-colors (count turn-order) invocation)
        [turn choices] (if cursor (choice/find-state game) [turn choices])
        {:keys [player-colors]} board]
    (game-layout
     [:main
      (flex-grow "row" 1)
      [:aside
       {:style
        {:width "30%"}}
       [organism-controls game board turn choices history]]
      [:article
       {:style {:flex-grow 1}}
       [organism-board game board invocation-colors turn choices]]
      [:nav
       {:style {:width "30%"}}
       [chat-panel description turn-order organism-victory invocation-colors player-colors player-captures state history cursor @chat]]])))

(defn create-game-input
  [player color]
  (let [game-key (r/atom "")]
    (fn []
      [:div
       {:style
        {:margin "30px 40px"}}
       [:h2
        [:span
         {:title "You can create a new game by entering any letters in this box and hitting enter."}
         "CREATE"]]
       [:input
        {:value @game-key
         :style
         {:border-radius "25px"
          :color "#fff"
          :background color
          :border "3px solid"
          :font-size "2em"
          :letter-spacing "8px"
          :margin "0px 20px"
          :padding "10px 40px"}
         :on-change
         (fn [event]
           (let [value (-> event .-target .-value)]
             (reset! game-key value)))
         :on-key-up
         (fn [event]
           (if (= (.-key event) "Enter")
             (set!
              (-> js/window .-location .-pathname)
              (str "/player/" player "/game/" @game-key))))}]])))

(defn open-games-section
  [player games]
  (when-not (empty? games)
    [:div
     {:style
      {:margin "20px 40px"}}
     [:h2
      [:span
       {:title "To join, click on an empty player field inside the create page for this game"}
       "OPEN"]]
     (for [{:keys [key invocation]} games]
       (let [{:keys [player-count players colors ring-count]} invocation
             colors (invocation-player-colors player-count invocation)
             player-color (first colors)]
         ^{:key key}
         [:div
          {:style
           {:margin "10px 20px"
            :padding "10px 0px"}}
          [:span
           [:a
            {:href (str "/player/" player "/game/" key)
             :style
             {:color "#fff"
              :border-radius "15px"
              :background player-color
              :padding "10px 20px"
              :letter-spacing "5px"
              :font-family font-choice
              :font-size "1.3em"}}
            key]]
          [:span
           {:style
            {:margin "0px 20px"}}
           " " ring-count " rings "]
          (for [[game-player color] (map vector players colors)]
            ^{:key game-player}
            [:span
             [:a
              {:href (str "/player/" player "/game/" key)
               :style
               (if (= game-player player)
                 {:color "#fff"
                  :border-radius "20px"
                  :background color
                  :margin "0px 10px"
                  :padding "7px 20px"}
                 {:padding "5px 10px"
                  :margin "0px 10px"
                  :border-style "solid"
                  :border-width "2px"
                  :border-color color
                  :border-radius "5px"
                  :color color})}
              game-player]])]))]))

(defn player-active?
  [player games]
  (let [active-games (get games "active")]
    (some?
     (some
      (fn [game]
        (= player (:current-player game)))
      active-games))))

(defn active-games-section
  [player games]
  (when-not (empty? games)
    [:div
     {:style
      {:margin "20px 40px"}}
     [:h2
      [:span
       {:title "A solid color row indicates it is your turn in that game.\nThe icon on the tab for this page will turn green when it is your turn."}
       "ACTIVE"]]
     (for [{:keys [game round players player-colors current-player]} games]
       (let [player-color (get player-colors player)]
         ^{:key game}
         [:div
          {:style
           (if (= player current-player)
             {:background player-color
              :margin "10px 20px"
              :padding "10px 0px"
              :border-radius "10px"}
             {:margin "10px 20px"
              :padding "10px 0px"})}
          [:span
           [:a
            {:href (str "/player/" player "/game/" game)
             :style
             {:color "#fff"
              :border-radius "15px"
              :background player-color
              :padding "10px 20px"
              :letter-spacing "5px"
              :font-family font-choice
              :font-size "1.3em"}}
            game]]
          [:span
           {:style
            {:margin "0px 20px"}}
           " round " (inc round)]
          (for [game-player players]
            (let [current-color (get player-colors game-player)]
              ^{:key game-player}
              [:span
               [:a
                {:href (str "/player/" game-player)
                 :style
                 (if (= game-player current-player)
                   {:color "#fff"
                    :border-radius "20px"
                    :background current-color
                    :margin "0px 10px"
                    :padding "7px 20px"}
                   {:padding "5px 10px"
                    :margin "0px 10px"
                    :border-style "solid"
                    :border-width "2px"
                    :border-color current-color
                    :border-radius "5px"
                    :color current-color})}
                game-player]]))]))]))

(defn complete-games-section
  [player games]
  (when-not (empty? games)
    [:div
     {:style
      {:margin "20px 40px"}}
     [:h2 "COMPLETE"]
     (for [{:keys [game round players player-colors winner]} (reverse games)]
       (let [player-color (get player-colors player)]
         ^{:key game}
         [:div
          {:style
           (if (= player winner)
             {:background player-color
              :margin "10px 20px"
              :padding "10px 0px"
              :border-radius "10px"}
             {:margin "10px 20px"
              :padding "10px 0px"})}
          [:span
           [:a
            {:href (str "/player/" player "/game/" game)
             :style
             {:color "#fff"
              :border-radius "15px"
              :background player-color
              :padding "10px 20px"
              :letter-spacing "5px"
              :font-family font-choice
              :font-size "1.3em"}}
            game]]
          [:span
           {:style
            {:margin "0px 20px"}}
           " round " round]
          (for [game-player players]
            (let [current-color (get player-colors game-player)]
              ^{:key game-player}
              [:span
               [:a
                {:href (str "/player/" game-player)
                 :style
                 (if (= game-player winner)
                   {:color "#fff"
                    :border-radius "20px"
                    :background current-color
                    :margin "0px 10px"
                    :padding "7px 20px"}
                   {:padding "5px 10px"
                    :margin "0px 10px"
                    :border-style "solid"
                    :border-width "2px"
                    :border-color current-color
                    :border-radius "5px"
                    :color current-color})}
                game-player]]))]))]))

(defn player-page
  [player]
  (let [games @player-games
        color (board/random-color 0.1 0.9)]
    [:div
     {:style
      {:margin "20px"}}
     [current-player-banner player color "games"]
     [create-game-input player color]
     [open-games-section player (get games "open")]
     [active-games-section player (get games "active")]
     [complete-games-section player (get games "complete")]]))

(defn valid-player-name?
  [players player]
  (and
   (not
    (empty? player))
   (not
    (players player))))

(defonce player-key
  (r/atom ""))

(defonce home-color
  (r/atom (board/random-color 0.5 0.8)))

(defn home-page
  [player-records]
  (let [color @home-color
        players (set (map :key player-records))
        active-color "#3b5"
        inactive-color "#444"]
    [:div
     [:div
      {:style
       {:color "#fff"
        :border-radius "50px"
        :cursor "pointer"
        :background color
        :letter-spacing "8px"
        :font-size "1.2em"
        :margin "20px 20px"
        :padding "25px 60px"}}
      [:h1 "ORGANISM"]
      [:h2 "welcome"]]
     [:div
      {:style
       {:margin "20px 20px"
        :padding "25px 60px"
        :font-size "1.2em"
        :font-family font-choice}}
      [:p "Welcome to ORGANISM!"]
      [:p "To begin, choose a player name ->"]
      [:input
       {:type :text
        :style
        {:border-radius "25px"
         :color "#fff"
         :background color
         :border "3px solid"
         :font-size "2em"
         :letter-spacing "8px"
         :margin "20px 20px"
         :padding "10px 40px"}
        :on-key-up
        (fn [event]
          (let [value (-> event .-target .-value)
                key (-> event .-key)]
            (reset! player-key value)
            (let [valid? (valid-player-name? players @player-key)]
              (if (and valid? (= key "Enter"))
                (dom/redirect!
                 (str "/player/" value))))))}]
      [:div
       (let [valid? (valid-player-name? players @player-key)]
         [:input
          {:type :button
           :value (if valid? "PLAY" "name taken")
           :style
           {:border-radius (if valid? "50px" "10px")
            :color "#fff"
            :cursor "pointer"
            :background (if valid? active-color inactive-color)
            :border "3px solid"
            :font-size "1.3em"
            :letter-spacing "8px"
            :margin "10px 30px"
            :padding "10px 40px"}
           :on-click
           (fn [event]
             (when (valid-player-name? players @player-key)
               (dom/redirect!
                (str "/player/" (-> event .-target .-value)))))}])]]]))

(def create-explanation
  [[:p "Every game has a unique key. A game will always be in one of three states: OPEN / ACTIVE / COMPLETE."]
   [:p "You can create a new game from your player page by entering any letters in the CREATE box and hitting enter."]
   [:p "From the create page, you can choose the number of rings and number of players, as well as the number of organisms required for victory."]
   [:p "You can also choose which other players will be in the game, as well as their personal capture limit required for victory (this defaults to 5)."]
   [:p "If you want to leave some player spots open for others to join, just leave them blank. It will show up in everyone's player page under OPEN."]
   [:p "To join an open game, simply click on the empty player slot and it will fill in your player name."]      
   [:p "Once all players have joined and you feel good about the game, hit the CREATE button to begin!"]])

(defn page-container
  []
  (if js/playerKey
    (if js/gameKey
      (let [invocation @board-invocation]
        (if (:created invocation)
          (let [game (:game @game-state)]
            (if (get-in game [:state :winner])
              [game-page]
              [game-page]))
          [create-page]))
      [player-page js/playerKey])
    [home-page (reader/read-string js/players)]))

(defn update-messages!
  [{:keys [type] :as received}]
  (println "MESSAGE RECEIVED" received)
  (condp = type
    "initialize"
    (do
      (swap! game-state initialize-game received)
      (reset! board-invocation (:invocation received))
      (reset! clear-state (-> received :game :state))
      (swap! chat initialize-chat received))
    "create"
    (do
      (reset! board-invocation (:invocation received))
      (reset! chat (:chat received))
      (apply-invocation! @board-invocation))
    "player-name"
    (let [{:keys [index player]} received]
      (swap! player-order assoc index player)
      (swap! board-invocation update :players (fn [players] (assoc (vec players) index player))))
    "game-state"
    (do
      (swap! game-state update-game received)
      (reset! food-source {})
      (swap!
       introduction
       (fn [introduction]
         (-> introduction
             (assoc :progress (-> received :game :state :player-turn :introduction))
             (assoc :chosen-element nil)
             (assoc :chosen-space nil)))))
    "chat" (swap! chat update-chat received)))

;; -------------------------
;; Routes

(def router
  (reitit/router
    [["/" :home]
     ["/about" :about]
     ["/player/:player"]
     ["/player/:player/game/:game"]]))

(defn match-route [uri]
  (->> (or (not-empty (string/replace uri #"^.*#" "")) "/")
       (reitit/match-by-path router)
       :data
       :name))
;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      HistoryEventType/NAVIGATE
      (fn [^js/Event.token event]
        (swap! session assoc :page (match-route (.-token event)))))
    (.setEnabled true)))

(defn mount-components
  []
  (println "MOUNTING")
  (rdom/render [#'page-container] (.getElementById js/document "organism")))

(def dormant-favicon "/favicon/dormant.ico")
(def active-favicon "/favicon/active.ico")
(def neutral-favicon "/favicon/neutral.ico")

(defn init!
  []
  (let [player? (not (empty? js/playerKey))
        game? (not (empty? js/gameKey))
        player-games? (and player? (not game?))
        observer? (and game? (not player?))
        player (if player? js/playerKey game/observer-key)]
    (println "intializing game" js/gameKey)
    (ajax/load-interceptors!)
    (hook-browser-navigation!)
    (let [protocol
          (if (= (.-protocol js/location) "https:")
            "wss:"
            "ws:")]
      (if js/playerGames
        (let [games (reader/read-string js/playerGames)
              favicon-path
              (if (player-active? player games)
                active-favicon
                dormant-favicon)]
          (dom/change-favicon favicon-path)
          (reset! player-games games)
          (.setInterval
           js/window
           (fn []
             (println "what")
             (.reload js/location))
           300000))
        (dom/change-favicon neutral-favicon))
      (when game?
        (ws/make-websocket!
         (str protocol "//" (.-host js/location) "/ws/player/" player "/game/" js/gameKey)
         update-messages!))
      (mount-components))))
