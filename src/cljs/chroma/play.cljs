(ns chroma.play
  "Thin reagent client for server-authoritative Chroma. All game logic lives on
   the server (organism.chroma.engine); this renders the precomputed `view` pushed
   over the websocket and sends place/swap/pass/chat messages back. Styled after the
   original HTML mock: card layout, reference mixing table, regions, bag, log,
   rules link, and an in-game bug report that ships the live game state."
  (:require
   [clojure.string :as string]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [ajax.core :refer [POST]]
   [organism.websockets :as ws]))

;; ── theme ────────────────────────────────────────────────────────────────────

(def C {:bg "#15171c" :panel "#1e2128" :ink "#e7e9ee" :dim "#9aa0ab" :line "#333a45"
        :board "#0e1014" :accent "#37d0c4" :gold "#f4d030"})

;; faint per-sector tints so the six wedges read on the empty board
(def sector-tint ["#1a1d24" "#191e22" "#1c1a22" "#1a2220" "#221a1c" "#1d2024"])

;; ── state ────────────────────────────────────────────────────────────────────

(defonce app (r/atom {:view nil :connected false :selected-chit nil :no-game false
                      :new-palette "CMY" :bug-open false :bug-text "" :bug-status nil}))

(defn- play-key [] (when (exists? js/playKey) js/playKey))
(defn- player-key [] (when (exists? js/playerKey) js/playerKey))
(defn- csrf [] (when (exists? js/csrfToken) js/csrfToken))

(def ^:const SCALE 26)

;; ── message senders (websocket) ──────────────────────────────────────────────

(defn- send! [msg] (ws/send-transit-message! msg))

(defn send-create! [palette]
  (send! {:type "create"
          :players [(or (player-key) "You") "Bot 2" "Bot 3"]
          :bots [1 2] :palette palette :depth 3 :trim true}))

(defn send-place! [c chit]
  (send! {:type "place" :c c :chit chit})
  (swap! app assoc :selected-chit nil))

(defn send-pass! [] (send! {:type "place" :pass true}))

(defn send-swap! [sw]
  (send! {:type "swap" :swap-type (:type sw) :discards (:discards sw) :get (:get sw)}))

(defn send-skip-swap! [] (send! {:type "swap" :skip true}))

;; ── bug report (HTTP POST, like the mock / eridu) ────────────────────────────

(defn submit-bug! []
  (let [text (:bug-text @app)]
    (when (seq (string/trim text))
      (swap! app assoc :bug-status :sending)
      (POST "/chroma/bug-report"
            {:params {:text text
                      :player (player-key)
                      :play-key (play-key)
                      :url (.-href js/location)
                      :ts (.toISOString (js/Date.))
                      :state (pr-str (:view @app))}
             :format :transit
             :response-format :transit
             :headers {"X-CSRF-Token" (csrf)}
             :handler (fn [_] (swap! app assoc :bug-status :sent :bug-text "")
                        (js/setTimeout #(swap! app assoc :bug-status nil :bug-open false) 1500))
             :error-handler (fn [_] (swap! app assoc :bug-status :error))}))))

;; ── geometry / colors ────────────────────────────────────────────────────────

(defn- hex-points [cx cy s]
  (->> (range 6)
       (map (fn [i]
              (let [a (* (/ Math/PI 180) (- (* 60 i) 30))]
                (str (+ cx (* s (Math/cos a))) "," (+ cy (* s (Math/sin a)))))))
       (string/join " ")))

(defn- rgb-css [[r g b]] (str "rgb(" r "," g "," b ")"))
(defn- chip-color [view color] (get-in view [:palette :chip color] "#888"))
(defn- color-name [view color] (get-in view [:palette :name color] color))

;; ── board ────────────────────────────────────────────────────────────────────

(defn- allowed-chits [you] (distinct (map :chit (:moves you))))
(defn- legal-cell? [you c] (some #(= % c) (:legal you)))

(defn- board-svg [view]
  (let [you (:you view)
        sel (:selected-chit @app)
        place? (and you (= "place" (:phase view)))
        wedge (:wedge you)
        legal? (fn [c] (and place? sel (legal-cell? you c)))]
    [:div {:style {:background (:board C) :border (str "1px solid " (:line C))
                   :border-radius "14px" :padding "8px" :box-shadow "inset 0 0 60px #000"}}
     [:svg {:viewBox "-360 -250 720 500" :width "100%" :style {:max-width "660px"}}
      (doall
       (for [cell (:board view)
             :let [c (:c cell)
                   cx (* SCALE (:x cell))
                   cy (* SCALE (:y cell))
                   stack (:stack cell)
                   col (:color cell)
                   in-wedge (and place? (= (:sector cell) wedge))
                   fill (cond
                          (:removed cell) "#0c0d10"
                          (empty? stack) (if in-wedge "#23303a" (nth sector-tint (mod (:sector cell) 6)))
                          (= col "mud") "#241f1b"
                          (= col "white") "#e8e8e0"
                          :else (rgb-css (:rgb cell)))]
             :when (not (:removed cell))]
         ^{:key (str c)}
         [:g {:on-click (when (legal? c) #(send-place! c sel))
              :style {:cursor (if (legal? c) "pointer" "default")}}
          [:polygon
           {:points (hex-points cx cy (- SCALE 1.5))
            :fill fill
            :stroke (cond (legal? c) (:gold C) in-wedge "#3a4754" :else "#2a2e36")
            :stroke-width (if (legal? c) 3 1)}]
          ;; stack-height pips
          (when (seq stack)
            [:text {:x cx :y (+ cy 4) :text-anchor "middle"
                    :style {:font-size "12px" :font-weight "800" :pointer-events "none"
                            :fill (if (= col "white") "#0009" "#000a")}}
             (count stack)])]))]]))

;; ── hand + swap ──────────────────────────────────────────────────────────────

(defn- chip [view ch {:keys [selected disabled on-click big]}]
  [:div {:on-click (when-not disabled on-click)
         :title (color-name view ch)
         :style {:width (if big "38px" "34px") :height (if big "38px" "34px")
                 :border-radius "8px" :background (chip-color view ch)
                 :border (if selected "3px solid #fff" "2px solid #0006")
                 :outline (when selected "1px solid #fff") :outline-offset "2px"
                 :opacity (if disabled 0.35 1)
                 :cursor (cond disabled "not-allowed" on-click "pointer" :else "default")
                 :display "flex" :align-items "center" :justify-content "center"
                 :font-weight "700" :color "#0009"}}
   ch])

(defn- hand-bar [view]
  (let [you (:you view)
        sel (:selected-chit @app)
        allowed (set (allowed-chits you))]
    [:div {:style {:margin "12px 0"}}
     [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap" :align-items "center"}}
      [:span {:style {:color (:dim C) :margin-right "4px" :font-size "12px"}} "YOUR HAND"]
      (for [[i ch] (map-indexed vector (:hand you))]
        ^{:key i}
        [chip view ch {:selected (= sel ch) :disabled (not (allowed ch))
                       :big true :on-click #(swap! app assoc :selected-chit ch)}])
      (when (:canPass you)
        [:button {:on-click send-pass! :style {:margin-left "8px"}} "Pass"])]
     [:div {:style {:color (:dim C) :font-size "12px" :margin-top "6px"}}
      (cond sel (str "Place " (color-name view sel) " on a highlighted cell.")
            (seq allowed) "Pick a chit, then click a glowing cell in your wedge."
            :else "No legal placement — you may pass.")]]))

(defn- swap-panel [view]
  (let [you (:you view)
        mud (:madeMud you)
        swaps (:availableSwaps you)]
    [:div {:style {:margin "12px 0" :padding "10px" :border-radius "8px"
                   :border (str "1px solid " (if mud (:gold C) (:line C)))
                   :background (if mud "#2a2614" "#23272f")}}
     [:div {:style {:color (:dim C) :margin-bottom "8px" :font-size "12px"}}
      (if mud "YOU MADE MUD — free 1-for-1 swap (discard 1, gain 1)"
          "OPTIONAL SWAP — discard 2, gain 1 (costs one net chit). Resolved simultaneously.")]
     (if (seq swaps)
       [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"}}
        (for [[i sw] (map-indexed vector swaps)]
          ^{:key i}
          [:button {:disabled (not (:playable sw)) :on-click #(send-swap! sw)
                    :title (if (:playable sw) "" "gained color is out of stock")
                    :style {:opacity (if (:playable sw) 1 0.4)
                            :border (str "1px solid " (if mud (:gold C) (:line C)))}}
           (str (string/join "+" (:discards sw)) " → " (:get sw))])]
       [:div {:style {:color (:dim C)}} "No swaps available."])
     [:button {:on-click send-skip-swap! :style {:margin-top "10px"}} "Skip swap →"]]))

;; ── cards ────────────────────────────────────────────────────────────────────

(defn- card [title & body]
  (into [:div {:style {:background (:panel C) :border (str "1px solid " (:line C))
                       :border-radius "10px" :padding "12px" :margin-bottom "12px"}}
         (when title
           [:h2 {:style {:font-size "11px" :text-transform "uppercase" :letter-spacing ".08em"
                         :color (:dim C) :margin "0 0 8px"}} title])]
        body))

(defn- stat-tile [label value]
  [:div {:style {:flex "1 1 60px" :background (:board C) :border (str "1px solid " (:line C))
                 :border-radius "8px" :padding "6px 8px" :text-align "center"}}
   [:div {:style {:font-size "20px" :font-weight "800"}} value]
   [:div {:style {:font-size "10px" :color (:dim C)}} label]])

(defn- scorebar [view]
  [card "Players"
   [:div {:style {:display "flex" :gap "8px" :flex-wrap "wrap"}}
    (for [[i s] (map-indexed vector (:seats view))
          :let [sc (when (:scores view) (nth (:scores view) (:seat s) nil))]]
      ^{:key i}
      [:div {:style {:flex "1 1 80px" :background (:board C) :border (str "1px solid "
                      (if (:waiting s) (:accent C) (:line C)))
                     :border-radius "8px" :padding "6px 8px"}}
       [:div {:style {:font-size "11px" :font-weight "700"}}
        (str (:name s) (when (:isBot s) " 🤖"))]
       [:div {:style {:font-size "20px" :font-weight "800"}}
        (if sc (:mult sc) (:handCount s))]
       [:div {:style {:font-size "10px" :color (:dim C)}}
        (if sc "points"
            (str (:handCount s) " chits · dry " (:dry s) "/3"
                 (when (:target s) (str " · ◎" (:target s)))))]])]])

(defn- regions-panel [view]
  [card "Largest region / color"
   [:div {:style {:display "flex" :gap "10px" :flex-wrap "wrap"}}
    (for [col (get-in view [:palette :order])]
      ^{:key col}
      [:div {:style {:display "flex" :align-items "center" :gap "4px"}}
       [:div {:style {:width "16px" :height "16px" :border-radius "4px"
                      :background (chip-color view col)}}]
       [:span {:style {:font-weight "700"}} (get-in view [:regions col] 0)]])]])

(defn- reference-table [view]
  [card "Mixing reference"
   [:table {:style {:border-collapse "collapse" :font-size "11px"}}
    [:tbody
     [:tr [:th {:style {:color (:dim C) :width "30px"}} "↓+→"]
      (for [col (get-in view [:palette :order])]
        ^{:key col}
        [:th {:style {:width "24px" :height "24px"}}
         [:div {:style {:width "16px" :height "16px" :border-radius "3px" :margin "auto"
                        :background (chip-color view col)}}]])]
     (for [row (:reference view)]
       ^{:key (:row row)}
       [:tr
        [:th {:style {:color (:dim C)}}
         (if (= (:row row) "blank") "·"
             [:div {:style {:width "16px" :height "16px" :border-radius "3px" :margin "auto"
                            :background (chip-color view (:row row))}}])]
        (for [cell (:cells row)]
          ^{:key (:col cell)}
          (let [res (:res cell)
                bg (cond (= res "mud") "#16181d" (= res "white") "#e8e8e0"
                         :else (chip-color view res))]
            [:td {:title (color-name view res)
                  :style {:border (str "1px solid " (:line C)) :width "24px" :height "24px"
                          :text-align "center" :background bg
                          :color "#0009" :font-weight "700" :font-size "9px"}}
             (if (= res "mud") "·" res)]))])]]])

(defn- bag-panel [view]
  [card (str "Bag · " (:bagTotal view) " left")
   [:div {:style {:display "flex" :gap "8px" :flex-wrap "wrap"}}
    (for [col (get-in view [:palette :order])]
      ^{:key col}
      [:div {:style {:display "flex" :align-items "center" :gap "3px"}}
       [:div {:style {:width "14px" :height "14px" :border-radius "3px"
                      :background (chip-color view col)}}]
       [:span (get-in view [:bag col] 0)]])]])

(defn- log-card [view]
  [card "Log"
   [:div {:style {:height "150px" :overflow "auto" :font-size "12px" :color (:dim C)
                  :white-space "pre-wrap" :background (:board C)
                  :border (str "1px solid " (:line C)) :border-radius "8px" :padding "8px"}}
    (for [[i line] (map-indexed vector (reverse (:log view)))]
      ^{:key i} [:div line])]])

;; ── modals / overlays ────────────────────────────────────────────────────────

(defn- bug-modal []
  (let [{:keys [bug-open bug-text bug-status]} @app]
    (when bug-open
      [:div {:style {:position "fixed" :inset 0 :background "#0b0d11ee" :z-index 60
                     :display "flex" :align-items "center" :justify-content "center" :padding "16px"}}
       [:div {:style {:background (:panel C) :border (str "1px solid " (:line C))
                      :border-radius "14px" :padding "20px" :max-width "440px" :width "100%"}}
        [:h2 {:style {:margin "0 0 8px"}} "Report a bug"]
        [:div {:style {:color (:dim C) :font-size "12px" :margin-bottom "8px"}}
         "Your note + the live game state are attached so it can be reproduced."]
        [:textarea {:value bug-text :rows 5
                    :on-change #(swap! app assoc :bug-text (.. % -target -value))
                    :placeholder "What happened?"
                    :style {:width "100%" :background (:board C) :color (:ink C)
                            :border (str "1px solid " (:line C)) :border-radius "8px" :padding "8px"}}]
        [:div {:style {:display "flex" :gap "8px" :margin-top "10px" :align-items "center"}}
         [:button {:class "on" :on-click submit-bug! :disabled (= bug-status :sending)}
          (case bug-status :sending "Sending…" :sent "Sent ✓" "Submit")]
         [:button {:on-click #(swap! app assoc :bug-open false)} "Cancel"]
         (when (= bug-status :error)
           [:span {:style {:color "#e88"}} "Failed — try again."])]]])))

(defn- game-over-overlay [view]
  (when (:over view)
    (let [scores (:scores view)
          seats (:seats view)
          winner (when (seq scores) (:seat (apply max-key #(:mult (nth scores (:seat %))) seats)))]
      [:div {:style {:position "fixed" :inset 0 :background "#0b0d11ee" :z-index 50
                     :display "flex" :align-items "center" :justify-content "center" :padding "16px"}}
       [:div {:style {:background (:panel C) :border (str "1px solid " (:line C))
                      :border-radius "14px" :padding "22px" :max-width "480px" :width "100%"}}
        [:h2 {:style {:margin "0 0 4px"}} "Game over"]
        [:div {:style {:color (:dim C) :margin-bottom "12px"}} (str (:turn view) " turns")]
        (for [[i s] (map-indexed vector seats)]
          ^{:key i}
          [:div {:style {:display "flex" :justify-content "space-between" :padding "8px 0"
                         :border-bottom (str "1px solid " (:line C))
                         :font-weight (if (= (:seat s) winner) "800" "400")}}
           [:span (str (:name s) (when (= (:seat s) winner) "  👑"))]
           [:span (str (:mult (nth scores (:seat s))) " pts")]])
        [:div {:style {:margin-top "16px"}}
         [:button {:class "on" :on-click #(send-create! (:new-palette @app))} "New game"]]]])))

;; ── header ───────────────────────────────────────────────────────────────────

(defn- header [view]
  [:div {:style {:display "flex" :gap "12px" :align-items "center" :flex-wrap "wrap"
                 :margin-bottom "12px"}}
   [:h1 {:style {:margin 0 :font-size "22px" :letter-spacing ".04em"}} "CHROMA"]
   [:div {:style {:display "flex" :gap "8px" :flex "1" :min-width "200px"}}
    [stat-tile "Turn" (:turn view)]
    [stat-tile "Bag" (:bagTotal view)]
    [stat-tile "Dry /3" (:maxDry view)]]
   [:div {:style {:display "flex" :gap "6px" :align-items "center" :flex-wrap "wrap"}}
    [:select {:value (:new-palette @app)
              :on-change #(swap! app assoc :new-palette (.. % -target -value))
              :style {:background "#2b2f38" :color (:ink C) :border (str "1px solid " (:line C))
                      :border-radius "8px" :padding "6px"}}
     [:option {:value "CMY"} "CMY + RGB"]
     [:option {:value "RYB"} "RYB + OGP"]]
    [:button {:on-click #(send-create! (:new-palette @app))} "New game"]
    [:a {:href "/chroma/rules" :target "_blank"
         :style {:color (:accent C) :text-decoration "none" :align-self "center"}} "Rules ↗"]
    [:button {:on-click #(swap! app assoc :bug-open true)} "Report a bug"]]])

;; ── top-level ────────────────────────────────────────────────────────────────

(defn- game-page [view]
  [:div
   [header view]
   [:div {:style {:display "flex" :gap "16px" :flex-wrap "wrap" :align-items "flex-start"}}
    [:div {:style {:flex "2" :min-width "320px"}}
     [board-svg view]
     (when (:you view)
       (case (:phase view)
         "place" [hand-bar view]
         "swap"  [swap-panel view]
         nil))
     (when (and (:you view) (:submitted (:you view)) (not (:over view)))
       [:div {:style {:color (:dim C) :margin "8px 0"}} "Waiting for other players…"])
     [log-card view]]
    [:div {:style {:flex "1" :min-width "260px"}}
     [scorebar view]
     [regions-panel view]
     [reference-table view]
     [bag-panel view]]]
   [bug-modal]
   [game-over-overlay view]])

(defn- new-game-page []
  [:div {:style {:text-align "center" :padding "48px"}}
   [:h1 {:style {:letter-spacing ".04em"}} "CHROMA"]
   [:p {:style {:color (:dim C)}} "No game here yet."]
   [:div {:style {:display "flex" :gap "8px" :justify-content "center" :align-items "center"}}
    [:select {:value (:new-palette @app)
              :on-change #(swap! app assoc :new-palette (.. % -target -value))
              :style {:background "#2b2f38" :color (:ink C) :border (str "1px solid " (:line C))
                      :border-radius "8px" :padding "8px"}}
     [:option {:value "CMY"} "CMY + RGB"]
     [:option {:value "RYB"} "RYB + OGP"]]
    [:button {:class "on" :on-click #(send-create! (:new-palette @app))
              :style {:padding "10px 20px" :font-size "16px"}}
     "Start a new game (you + 2 bots)"]]
   [bug-modal]])

(defn root []
  (let [{:keys [view no-game connected]} @app]
    [:div {:style {:font-family "system-ui, Segoe UI, Roboto, sans-serif" :color (:ink C)
                   :background (:bg C) :min-height "100vh" :padding "14px"
                   :max-width "1100px" :margin "0 auto"}}
     (cond
       (not connected) [:div {:style {:color (:dim C) :padding "40px" :text-align "center"}} "Connecting…"]
       no-game [new-game-page]
       view [game-page view]
       :else [:div {:style {:color (:dim C) :padding "40px" :text-align "center"}} "Loading…"])]))

;; ── ws wiring ────────────────────────────────────────────────────────────────

(defn- handle-message [msg]
  (case (:type msg)
    "game-state" (swap! app assoc :view msg :no-game false)
    "no-game"    (swap! app assoc :no-game true :view nil)
    nil))

(defn- ws-url []
  (str (if (= "https:" (.-protocol js/location)) "wss://" "ws://")
       (.-host js/location) "/ws/chroma/play/" (play-key)))

(defn mount-components []
  (rdom/render [root] (.getElementById js/document "chroma")))

(defn init! []
  (ws/make-websocket! (ws-url) handle-message (fn [] (swap! app assoc :connected true)))
  (mount-components))
