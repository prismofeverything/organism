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

;; Empty hexes are now WHITE (ChromaBoardEdit.png target), so the six wedges can no
;; longer read via a dark fill tint — they read via the cell OUTLINE colour instead.
;; Muted per-sector hues, visible as a thin border on the white board.
(def sector-line ["#9b7fb0" "#7f93b0" "#5fa888" "#b0a86a" "#b0897f" "#8f7fb0"])

;; ── state ────────────────────────────────────────────────────────────────────

;; :setup is the Eridu-style create config: a palette + a per-seat vector of
;; {:type "human"/"bot" :name "..."}. Seat count is just (count seats). Seat 0
;; defaults to you; the server already accepts arbitrary :players/:bots on create.
(defonce app (r/atom {:view nil :connected false :selected-chit nil :no-game false
                      :setup {:palette "CMY"
                              :seats [{:type "human" :name nil}
                                      {:type "bot" :name nil}
                                      {:type "bot" :name nil}]}
                      :setup-open false
                      :bug-open false :bug-text "" :bug-status nil}))

(defn- play-key [] (when (exists? js/playKey) js/playKey))
(defn- player-key [] (when (exists? js/playerKey) js/playerKey))
(defn- csrf [] (when (exists? js/csrfToken) js/csrfToken))

(def ^:const SCALE 26)

;; ── message senders (websocket) ──────────────────────────────────────────────

(defn- send! [msg] (ws/send-transit-message! msg))

(def ^:const MIN-SEATS 1)
(def ^:const MAX-SEATS 6)

(defn- seat-name
  "Resolve the name a seat is created under. Seat 0 defaults to you so you match
   your seat; bots and unnamed humans get a generic label the server also tolerates."
  [i {:keys [type name]}]
  (let [nm (some-> name string/trim)]
    (cond
      (seq nm)         nm
      (= i 0)          (or (player-key) "You")
      (= type "bot")   (str "Bot " (inc i))
      :else            (str "Player " (inc i)))))

(defn- config->create-msg
  "Turn the :setup config into the websocket create message the server expects."
  [{:keys [palette seats]}]
  (let [seats (vec seats)]
    {:type "create"
     :players (vec (map-indexed seat-name seats))
     :bots (vec (keep-indexed (fn [i s] (when (= (:type s) "bot") i)) seats))
     :palette palette :depth 3 :trim true}))

(defn send-create!
  "Create a game from the current setup config (or an explicit one)."
  ([] (send-create! (:setup @app)))
  ([config]
   (send! (config->create-msg config))
   (swap! app assoc :setup-open false)))

;; ── setup-config editing helpers ──────────────────────────────────────────────

(defn- set-seat-count! [n]
  (swap! app update-in [:setup :seats]
         (fn [seats]
           (let [n (max MIN-SEATS (min MAX-SEATS n))
                 seats (vec seats)]
             (cond
               (= n (count seats)) seats
               (< n (count seats)) (subvec seats 0 n)
               :else (into seats (repeat (- n (count seats)) {:type "bot" :name nil})))))))

(defn- set-seat-type! [i t] (swap! app assoc-in [:setup :seats i :type] t))
(defn- set-seat-name! [i nm] (swap! app assoc-in [:setup :seats i :name] nm))
(defn- set-palette! [p] (swap! app assoc-in [:setup :palette] p))

(defn send-place!
  ;; visibility rule: fromHidden says to spend a hidden starting chit (vs an open one)
  ([c chit] (send-place! c chit false))
  ([c chit fromHidden]
   (send! {:type "place" :c c :chit chit :fromHidden (boolean fromHidden)})
   (swap! app assoc :selected-chit nil)))

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
                   occupied (seq stack)
                   in-wedge (and place? (= (:sector cell) wedge))
                   ;; ChromaBoardEdit.png target: empty hexes WHITE, occupied = colour
                   ;; tiles, removed = outlined X. Dark centre hub is automatic — the
                   ;; [0 0] "K" seed is an occupied stack that classifies as mud.
                   fill (cond
                          (:removed cell) "none"
                          (not occupied)  "#f5f7f9"          ; white empty
                          (= col "mud")   "#241f1b"
                          (= col "white") "#e8e8e0"          ; off-white so a white STACK still reads as a tile
                          :else (rgb-css (:rgb cell)))
                   ;; with white fills, the sector + active-wedge cues live in the OUTLINE
                   stroke (cond
                            (legal? c)      (:gold C)
                            (:removed cell) "#2a2e36"
                            occupied        "#11151b"        ; dark border ⇒ occupied tile reads against white
                            in-wedge        (:accent C)      ; active wedge cue
                            :else           (nth sector-line (mod (:sector cell) 6)))
                   sw (cond (legal? c) 3 occupied 1.5 in-wedge 2 :else 1.5)]]
         ^{:key (str c)}
         [:g {:on-click (when (legal? c) #(send-place! c (:ch sel) (:hidden? sel)))
              :style {:cursor (if (legal? c) "pointer" "default")}}
          [:polygon
           {:points (hex-points cx cy (- SCALE 1.5))
            :fill fill :stroke stroke :stroke-width sw}]
          ;; removed cells render as an outlined X (mockup), not a hole
          (when (:removed cell)
            [:text {:x cx :y (+ cy 5) :text-anchor "middle"
                    :style {:font-size "15px" :font-weight "800" :pointer-events "none" :fill "#2a2e36"}}
             "✕"])
          ;; stack-height pips
          (when occupied
            [:text {:x cx :y (+ cy 4) :text-anchor "middle"
                    :style {:font-size "12px" :font-weight "800" :pointer-events "none"
                            :fill (if (= col "white") "#0009" "#000a")}}
             (count stack)])]))]]))

;; ── hand + swap ──────────────────────────────────────────────────────────────

(defn- chip [view ch {:keys [selected disabled on-click big hidden mini title]}]
  [:div {:on-click (when-not disabled on-click)
         :title (or title (color-name view ch))
         :style {:width (cond mini "18px" big "38px" :else "34px")
                 :height (cond mini "18px" big "38px" :else "34px")
                 :border-radius (if mini "5px" "8px") :background (chip-color view ch)
                 ;; visibility rule: a HIDDEN starting chit renders with a dashed ring
                 :border (cond selected "3px solid #fff"
                               hidden "2px dashed #ffffffaa"
                               :else "2px solid #0006")
                 :outline (when selected "1px solid #fff") :outline-offset "2px"
                 :opacity (if disabled 0.35 1)
                 :cursor (cond disabled "not-allowed" on-click "pointer" :else "default")
                 :display "flex" :align-items "center" :justify-content "center"
                 :font-size (when mini "10px")
                 :font-weight "700" :color "#0009"}}
   ch])

(defn- hand-bar [view]
  (let [you (:you view)
        place? (= "place" (:phase view))
        sel (:selected-chit @app)
        allowed (set (allowed-chits you))
        pub (or (:pub you) {})]
    [:div {:style {:margin "12px 0"}}
     [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap" :align-items "center"}}
      [:span {:style {:color (:dim C) :margin-right "4px" :font-size "12px" :font-weight "700"}} "YOUR HAND"]
      ;; visibility rule: per color the first pub[c] instances (hand order) are OPEN
      ;; (everyone sees them); the rest are hidden starting chits (dashed). Clicking a
      ;; specific chit chooses which pool you spend from.
      (let [seen (atom {})]
        (doall
         (for [[i ch] (map-indexed vector (:hand you))]
           (let [inst (get @seen ch 0)
                 _ (swap! seen update ch (fnil inc 0))
                 hidden? (>= inst (get pub ch 0))]
             ^{:key i}
             [chip view ch {:selected (and place? (= (:ch sel) ch) (= (:hidden? sel) hidden?))
                            :disabled (or (not place?) (not (allowed ch)))
                            :big true :hidden hidden?
                            :title (str (color-name view ch)
                                        (if hidden? " — hidden starting chit (spending it burns the secret)"
                                            " — open (everyone can see you hold this)"))
                            :on-click (when place? #(swap! app assoc :selected-chit {:ch ch :hidden? hidden?}))}]))))
      (when (and place? (:canPass you))
        [:button {:on-click send-pass! :style {:margin-left "8px"}} "Pass"])]
     [:div {:style {:color (:dim C) :font-size "12px" :margin-top "6px"}}
      (cond
        (not place?) "Placement locked in — resolve your swap below (or skip)."
        sel (str "Place " (color-name view (:ch sel))
                 (if (:hidden? sel) " (from your hidden hand)" " (an open chit)")
                 " on a highlighted cell.")
        (seq allowed) "Pick a chit, then click a glowing cell in your wedge. Dashed = hidden starting chit."
        :else "No legal placement — you may pass.")]]))

;; Swap area is ALWAYS on screen alongside the hand. Its options stay visible at all
;; times (so you can plan), but are only clickable during the swap step (:canSwap) —
;; honouring place-then-swap sequencing. A mud turn upgrades it to the glowing SUPER
;; SWAP (discard 1, gain 1, free).
(defn- swap-area [view]
  (let [you (:you view)
        can? (:canSwap you)
        mud (:madeMud you)
        swaps (:availableSwaps you)]
    [:div {:class (when mud "superswap")
           :style {:margin "12px 0" :padding "10px" :border-radius "10px"
                   :border (str "2px solid " (cond mud (:gold C) can? (:accent C) :else (:line C)))
                   :background (cond mud "linear-gradient(135deg,#2e2810,#231f12)"
                                     can? "#1b2226" :else "#16181d")
                   :transition "border-color .2s, background .2s"}}
     [:div {:style {:display "flex" :align-items "center" :gap "8px" :margin-bottom "8px" :flex-wrap "wrap"}}
      (if mud
        [:span {:style {:font-weight "800" :color (:gold C) :font-size "15px" :letter-spacing ".03em"}}
         "⚡ SUPER SWAP"]
        [:span {:style {:font-weight "700" :color (:dim C) :font-size "12px"}} "SWAP"])
      [:span {:style {:color (if mud "#ecdca6" (:dim C)) :font-size "12px"}}
       (cond
         mud  "You made mud — discard 1, gain 1. Free, this turn only!"
         can? "Discard 2, gain 1 (costs one net chit). Resolved simultaneously."
         :else "Available after you place — sequence of play.")]]
     (if (seq swaps)
       [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"}}
        (for [[i sw] (map-indexed vector swaps)]
          ^{:key i}
          [:button {:disabled (or (not can?) (not (:playable sw)))
                    :on-click (when can? #(send-swap! sw))
                    :title (cond (not can?) "resolve after placing"
                                 (not (:playable sw)) "gained color is out of stock" :else "")
                    :style {:opacity (if (and can? (:playable sw)) 1 0.4)
                            :border (str "1px solid " (if mud (:gold C) (:line C)))
                            :font-weight (if mud "700" "400")
                            :color (when mud "#2a2410")
                            :background (when mud (:gold C))}}
           (str (string/join "+" (:discards sw)) " → " (:get sw))])]
       [:div {:style {:color (:dim C)}} "No swaps available from this hand."])
     (when can?
       [:button {:on-click send-skip-swap! :style {:margin-top "10px"}} "Skip swap →"])]))

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
       ;; visibility rule: this seat's PUBLIC (drawn) chits + how many stay hidden
       (when-not sc
         (let [pub (or (:pub s) {})
               pub-chits (for [col (get-in view [:palette :order])
                               n (range (get pub col 0))]
                           [col n])
               n-hidden (- (:handCount s) (count pub-chits))]
           [:div {:style {:display "flex" :gap "3px" :flex-wrap "wrap" :align-items "center" :margin "3px 0"}}
            (doall (for [[col n] pub-chits]
                     ^{:key (str col n)} [chip view col {:mini true :disabled false}]))
            (when (pos? n-hidden)
              [:span {:style {:font-size "10px" :color (:dim C)}} (str n-hidden " hidden")])]))
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

(defn- score-breakdown
  "Chips showing which colors this seat won + each value, then mud. * marks a tie
   (scored the 2nd-largest region of that color)."
  [view sc]
  [:div {:style {:display "flex" :gap "10px" :flex-wrap "wrap" :align-items "center" :margin-top "5px"}}
   (let [cols (filter #(> (:value %) 0) (get-in sc [:breakdown :colors]))]
     (if (seq cols)
       (for [[i cb] (map-indexed vector cols)]
         ^{:key i}
         [:span {:style {:display "inline-flex" :align-items "center" :gap "4px"}
                 :title (str (color-name view (:color cb)) (when (:tie cb) " (tie → 2nd-largest)"))}
          [:span {:style {:width "14px" :height "14px" :border-radius "3px"
                          :background (chip-color view (:color cb))}}]
          [:span {:style {:font-size "12px" :font-variant-numeric "tabular-nums"}}
           (:value cb) (when (:tie cb) "*")]])
       [:span {:style {:color (:dim C) :font-size "12px"}} "no colors"]))
   (when (and (get-in sc [:breakdown :mud]) (pos? (get-in sc [:breakdown :mud :value])))
     [:span {:style {:display "inline-flex" :align-items "center" :gap "4px"}
             :title "mud — fewest chips in hand"}
      [:span {:style {:width "14px" :height "14px" :border-radius "3px" :background "#3a322a"}}]
      [:span {:style {:font-size "12px" :color "#c8b79a"}}
       "mud " (get-in sc [:breakdown :mud :value]) (when (get-in sc [:breakdown :mud :tie]) "*")]])])

(defn- game-over-overlay [view]
  (when (and (:over view) (not (:results-hidden @app)))
    (let [scores (:scores view)
          seats (:seats view)
          winner (when (seq scores) (:seat (apply max-key #(:mult (nth scores (:seat %))) seats)))]
      [:div {:style {:position "fixed" :inset 0 :background "#0b0d11ee" :z-index 50
                     :display "flex" :align-items "center" :justify-content "center" :padding "16px"}}
       [:div {:style {:background (:panel C) :border (str "1px solid " (:line C))
                      :border-radius "14px" :padding "22px" :max-width "560px" :width "100%"
                      :max-height "86vh" :overflow-y "auto"}}
        [:h2 {:style {:margin "0 0 2px"}} "Game over"]
        [:div {:style {:color (:dim C) :margin-bottom "12px" :font-size "13px"}}
         (str (:turn view) " turns · op2b scoring — each color scores for its biggest holder")]
        (for [[i s] (map-indexed vector seats)
              :let [sc (nth scores (:seat s))
                    win? (= (:seat s) winner)]]
          ^{:key i}
          [:div {:style {:padding "10px 0" :border-bottom (str "1px solid " (:line C))}}
           [:div {:style {:display "flex" :justify-content "space-between" :align-items "baseline"}}
            [:span {:style {:font-weight (if win? "800" "600")}}
             (str (:name s) (when (:isBot s) " 🤖") (when win? "  👑"))]
            [:span {:style {:font-weight "800" :font-size "18px"}} (str (:mult sc) " pts")]]
           [score-breakdown view sc]
           [:div {:style {:color (:dim C) :font-size "11px" :margin-top "4px"}}
            (str "regions " (:regionScore sc)
                 (when (pos? (:mudScore sc)) (str " + mud " (:mudScore sc))))]])
        [:div {:style {:color (:dim C) :font-size "11px" :margin "10px 0 2px"}}
         "* = tie for most chips → scored that color's 2nd-largest region."]
        [:div {:style {:margin-top "14px" :display "flex" :gap "8px" :flex-wrap "wrap"}}
         [:button {:class "on" :on-click #(swap! app assoc :setup-open true)} "New game"]
         [:a {:href "/chroma/lobby"
              :style {:align-self "center" :color (:accent C) :text-decoration "none"}} "Lobby"]
         [:button {:on-click #(swap! app assoc :results-hidden true)} "View board"]]]])))

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
    [:a {:href "/chroma/lobby"
         :style {:color (:accent C) :text-decoration "none" :align-self "center"}} "Lobby"]
    [:button {:on-click #(swap! app assoc :setup-open true)} "New game"]
    [:a {:href "/chroma/rules" :target "_blank"
         :style {:color (:accent C) :text-decoration "none" :align-self "center"}} "Rules ↗"]
    [:button {:on-click #(swap! app assoc :bug-open true)} "Report a bug"]]])

;; ── setup / create UI (Eridu-style: palette + player count + per-seat) ────────

(defn- palette-select [palette]
  [:select {:value palette
            :on-change #(set-palette! (.. % -target -value))
            :style {:background "#2b2f38" :color (:ink C) :border (str "1px solid " (:line C))
                    :border-radius "8px" :padding "8px"}}
   [:option {:value "CMY"} "CMY + RGB"]
   [:option {:value "RYB"} "RYB + OGP"]])

(defn- seg-button [on? label on-click]
  [:button {:on-click on-click
            :style {:padding "6px 12px" :border-radius "8px" :cursor "pointer"
                    :border (str "1px solid " (if on? (:accent C) (:line C)))
                    :background (if on? (:accent C) "#2b2f38")
                    :color (if on? "#06262a" (:ink C))
                    :font-weight (if on? "700" "400")}}
   label])

(defn- setup-panel
  "Palette + player-count + per-seat human/bot assignment. Wires straight to the
   create message the server already accepts (arbitrary :players/:bots)."
  [{:keys [title on-cancel]}]
  (let [{:keys [palette seats]} (:setup @app)
        n (count seats)]
    [:div {:style {:max-width "420px" :margin "0 auto" :text-align "left"}}
     (when title [:h2 {:style {:margin "0 0 12px" :text-align "center"}} title])

     [:div {:style {:margin-bottom "14px"}}
      [:div {:style {:color (:dim C) :font-size "12px" :font-weight "700" :margin-bottom "6px"}} "PALETTE"]
      [palette-select palette]]

     [:div {:style {:margin-bottom "14px"}}
      [:div {:style {:color (:dim C) :font-size "12px" :font-weight "700" :margin-bottom "6px"}} "PLAYERS"]
      [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"}}
       (for [k (range MIN-SEATS (inc MAX-SEATS))]
         ^{:key k} [seg-button (= k n) (str k) #(set-seat-count! k)])]]

     [:div {:style {:margin-bottom "16px"}}
      [:div {:style {:color (:dim C) :font-size "12px" :font-weight "700" :margin-bottom "6px"}} "SEATS"]
      (for [[i s] (map-indexed vector seats)]
        ^{:key i}
        [:div {:style {:display "flex" :gap "8px" :align-items "center" :margin-bottom "6px"}}
         [:span {:style {:width "54px" :color (:dim C) :font-size "13px"}} (str "Seat " (inc i))]
         [seg-button (= (:type s) "human") "Human" #(set-seat-type! i "human")]
         [seg-button (= (:type s) "bot") "🤖 Bot" #(set-seat-type! i "bot")]
         (when (= (:type s) "human")
           [:input {:value (or (:name s) "")
                    :placeholder (if (= i 0) (str "you" (when (player-key) (str " (" (player-key) ")"))) (str "Player " (inc i)))
                    :on-change #(set-seat-name! i (.. % -target -value))
                    :style {:flex "1" :min-width "0" :background (:board C) :color (:ink C)
                            :border (str "1px solid " (:line C)) :border-radius "8px" :padding "6px 8px"}}])])]

     [:div {:style {:display "flex" :gap "8px" :justify-content "center" :margin-top "6px"}}
      [:button {:class "on" :on-click #(send-create!)
                :style {:padding "10px 20px" :font-size "15px"}} "Start game"]
      (when on-cancel [:button {:on-click on-cancel} "Cancel"])]
     [:div {:style {:color (:dim C) :font-size "11px" :text-align "center" :margin-top "10px"}}
      "Human seats join by logging in under the seat's name; bots play automatically."]]))

(defn- setup-modal []
  (when (:setup-open @app)
    [:div {:style {:position "fixed" :inset 0 :background "#0b0d11ee" :z-index 60
                   :display "flex" :align-items "center" :justify-content "center" :padding "16px"}}
     [:div {:style {:background (:panel C) :border (str "1px solid " (:line C))
                    :border-radius "14px" :padding "22px" :max-width "460px" :width "100%"
                    :max-height "90vh" :overflow-y "auto"}}
      [setup-panel {:title "New game" :on-cancel #(swap! app assoc :setup-open false)}]]]))

;; ── top-level ────────────────────────────────────────────────────────────────

(defn- game-page [view]
  [:div
   [header view]
   [:div {:style {:display "flex" :gap "16px" :flex-wrap "wrap" :align-items "flex-start"}}
    [:div {:style {:flex "2" :min-width "320px"}}
     [board-svg view]
     (when (and (:you view) (not (:over view)))
       [:div
        [hand-bar view]
        [swap-area view]])
     (when (and (:you view) (:submitted (:you view)) (not (:over view)))
       [:div {:style {:color (:dim C) :margin "8px 0"}} "Waiting for other players…"])
     [log-card view]]
    [:div {:style {:flex "1" :min-width "260px"}}
     [scorebar view]
     [regions-panel view]
     [reference-table view]
     [bag-panel view]]]
   [bug-modal]
   [setup-modal]
   [game-over-overlay view]
   ;; when results are dismissed to inspect the final board, offer a way back
   (when (and (:over view) (:results-hidden @app))
     [:button {:on-click #(swap! app assoc :results-hidden false)
               :style {:position "fixed" :bottom "16px" :right "16px" :z-index 51
                       :padding "10px 16px" :background (:accent C) :color "#06262a"
                       :border "none" :border-radius "10px" :font-weight "700"
                       :cursor "pointer" :box-shadow "0 4px 16px #0008"}}
      "Show results ▸"])])

(defn- new-game-page []
  [:div {:style {:text-align "center" :padding "40px 16px"}}
   [:h1 {:style {:letter-spacing ".04em"}} "CHROMA"]
   [:p {:style {:color (:dim C) :margin-bottom "20px"}} "Set up a new game on this table."]
   [setup-panel {:title nil}]
   [:div {:style {:margin-top "18px"}}
    [:a {:href "/chroma/lobby"
         :style {:color (:accent C) :text-decoration "none"}} "← back to lobby"]]
   [bug-modal]])

(defn root []
  (let [{:keys [view no-game connected]} @app]
    [:div {:style {:font-family "system-ui, Segoe UI, Roboto, sans-serif" :color (:ink C)
                   :background (:bg C) :min-height "100vh" :padding "14px"
                   :max-width "1100px" :margin "0 auto"}}
     [:style "@keyframes chroma-pulse{0%,100%{box-shadow:0 0 5px #f4d03055}50%{box-shadow:0 0 20px #f4d030cc}}
              .superswap{animation:chroma-pulse 1.15s ease-in-out infinite}
              .superswap button:not(:disabled):hover{filter:brightness(1.12)}"]
     (cond
       (not connected) [:div {:style {:color (:dim C) :padding "40px" :text-align "center"}} "Connecting…"]
       no-game [new-game-page]
       view [game-page view]
       :else [:div {:style {:color (:dim C) :padding "40px" :text-align "center"}} "Loading…"])]))

;; ── ws wiring ────────────────────────────────────────────────────────────────

(defn- handle-message [msg]
  (case (:type msg)
    "game-state" (swap! app (fn [s] (cond-> (assoc s :view msg :no-game false)
                                       ;; reopen the results panel for each new finish
                                       (not (:over msg)) (assoc :results-hidden false))))
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
