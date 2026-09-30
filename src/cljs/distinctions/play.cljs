(ns distinctions.play
  "The DISTINCTIONS table.

   Globals set by the HTML shell, as every game here does it:

     js/playKey   — /distinctions/play/:play   a game (WebSocket)
     js/playerKey — who you are, or --observer--
     js/isCreate  — /distinctions/create
     js/isObserve — /distinctions/observe
     js/isGames   — /distinctions/play

   The state arrives already cut down by the server: other hands are just
   counts until the game is over, and the deck is only a number."
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.ajax :as ajax]
   [organism.components :as components]
   [organism.websockets :as ws]
   [distinctions.card :as card]
   [distinctions.game :as game]))

(defonce game-state (r/atom nil))
(defonce chat-lines (r/atom []))

(def ground "#1c1b20")
(def panel  "#26252b")
(def ink    "#e8e4da")
(def faint  "#8a8690")
(def accent "#e8e4da")
(def red    "#d42a20")

(defn- safe-read [s]
  (when (and s (not (str/blank? s)))
    (try (reader/read-string s) (catch :default _ nil))))

;; ── Connection ─────────────────────────────────────────────────────────────

(defn- receive-message! [message]
  (case (get message "type")
    ("initialize" "game-state")
    (do (when-let [st (safe-read (get message "state"))] (reset! game-state st))
        (when-let [c (safe-read (get message "chat"))] (reset! chat-lines c)))
    (js/console.log "distinctions: unknown message" (get message "type"))))

(defn connect-ws! [pk]
  (let [proto (if (= "https:" (.-protocol js/location)) "wss:" "ws:")]
    (ws/make-websocket! (str proto "//" (.-host js/location) "/ws/distinctions/play/" pk)
                        receive-message!)))

(defn- send-action! [action]
  (ws/send-transit-message! {"type" "action" "choice" (pr-str action)}))

(defn- send-start! [] (ws/send-transit-message! {"type" "start"}))

(defn- send-chat! [line] (ws/send-transit-message! {"type" "chat" "line" line}))

;; ── Pieces ─────────────────────────────────────────────────────────────────

(defn- made-words
  "The two winning teams: \"black · red · circle  /  eye · rays · plain\"."
  [made]
  (str/join "  /  " (for [{:keys [shared]} made]
                      (str/join " · " (map game/target-name shared)))))

(def ^:private goal-words "two teams of four, covering all six")

(defn- solo? [state] (= 1 (count (:players state))))

(defn- button [label on-click & [primary?]]
  [:button {:on-click on-click
            :style {:background (if primary? accent "#34323a")
                    :color (if primary? ground ink)
                    :border "none" :border-radius "6px" :padding "10px 22px"
                    :cursor "pointer" :font-family "monospace" :font-size "14px"
                    :letter-spacing "1px"}}
   label])

(defn- opponent [state {:keys [seat name]}]
  (let [acting? (= seat (:to-act state))
        n       (get-in state [:hand-counts seat] 0)
        shown   (get-in state [:hands seat])
        winner? (= name (:winner state))]
    [:div {:style {:background (if acting? "#302c22" panel)
                   :border (str "1px solid " (cond winner? red acting? "#8a7a4a" :else "#34323a"))
                   :border-radius "8px" :padding "10px 12px" :min-width "150px"}}
     [:div {:style {:font-size "14px" :margin-bottom "8px"
                    :color (if acting? "#f0d890" ink)}}
      name
      [:span {:style {:color faint :margin-left "8px" :font-size "12px"}}
       (cond winner? "wins"
             acting? (if (= :draw (:step state)) "drawing…" "discarding…")
             :else (str n " cards"))]]
     [:div {:style {:display "flex"}}
      (if shown
        (let [teams? (and winner? (seq (:made state)))
              order  (if teams? (mapcat :cards (:made state)) (sort shown))]
          (for [[i c] (map-indexed vector order)]
            ^{:key i} [:div {:style {:margin-right (if (and teams? (= i 3)) "12px" "3px")}}
                       [card/card {:value c :width 30}]]))
        (for [i (range n)]
          ^{:key i} [:div {:style {:margin-right "-16px"}} [card/back {:width 30}]]))]]))

(defn- centre [state]
  (let [{:keys [draw take]} (:actions state)
        top (game/top-discard state)
        lift {:cursor "pointer" :outline (str "2px solid " accent) :outline-offset "3px"}]
    [:div {:style {:display "flex" :gap "40px" :justify-content "center"
                   :align-items "flex-end" :margin "28px 0"}}
     [:div {:style {:text-align "center"}}
      [card/back {:width 96
                  :props (when draw {:on-click #(send-action! {:action :draw})
                                     :style lift})}]
      [:div {:style {:color faint :font-size "12px" :margin-top "8px"}}
       (str "deck · " (:deck-count state))]]
     [:div {:style {:text-align "center"}}
      (if top
        [card/card {:value top :width 96
                    :props (when take {:on-click #(send-action! {:action :take})
                                       :style lift})}]
        [:div {:style {:width "96px" :height "134px" :border "1px dashed #44424a"
                       :border-radius "5px"}}])
      [:div {:style {:color faint :font-size "12px" :margin-top "8px"}}
       (str "pile · " (count (:discard state)))]]]))

(defonce selected (r/atom #{}))

(def ^:private legend-rows
  "The key's six rows, in its order. Background and foreground are swatches;
   the rest are sample cards that build up the way the key's do, each pair
   differing only in its own distinction: white/red circle, then + eye, then
   + rays, then inverted."
  [[:background  :swatch]
   [:foreground  :swatch]
   [:composition 32]
   [:eye         32]
   [:rays        36]
   [:inversion   38]])

(defn- with-value
  "`card` with distinction k set to v."
  [card k v]
  (let [bit (bit-shift-left 1 (:bit (game/axis-by-key k)))]
    (if (= 1 v) (bit-or card bit) (bit-and card (bit-not bit)))))

(defn- sample
  "The sample card for value v of row k. The key builds its card up one row
   at a time, and so does this: each row wears whatever the selected cards
   have in common on the rows *above* it -- select blue cards and the samples
   turn blue -- keeps the key's own sample for the rows below, and shows v on
   its own. So only the last row, inversion, can be the selected card itself."
  [base k v common]
  (let [above (set (take-while #(not= k %) (map first legend-rows)))]
    (with-value (reduce (fn [c [ck cv]] (if (above ck) (with-value c ck cv) c)) base common)
                k v)))

(def ^:private swatch-colour
  {[:background 0] (card/bw "black") [:background 1] (card/bw "white")
   [:foreground 0] (card/rb "red")   [:foreground 1] (card/rb "blue")})

(defn- legend-tile
  "One value of one distinction, lit when every selected card holds it."
  [k v sample n total]
  (let [all?  (and (pos? total) (= n total))
        some? (pos? n)]
    [:div {:style {:width "74px" :display "flex" :flex-direction "column"
                   :align-items "center" :gap "3px"
                   ;; once anything is selected, only what all of it shares stays lit
                   :opacity (if (and (pos? total) (not all?)) 0.28 1)
                   :transition "opacity 150ms"}}
     [:div {:style {:padding "3px" :border-radius "7px"
                    :box-shadow (when all? (str "0 0 0 2px " accent))}}
      (if (= sample :swatch)
        [:div {:style {:width "30px" :height "30px" :border-radius "50%"
                       :background (swatch-colour [k v])
                       :box-shadow "inset 0 0 0 1px #55525c"}}]
        [card/card {:value sample :width 30}])]
     [:div {:style {:font-size "11px" :color (if all? ink faint) :white-space "nowrap"}}
      (game/value-name k v)
      (when (and some? (not all?))
        [:span {:style {:color ink :margin-left "4px"}} n])]]))

(defn- legend
  "The key, live: where the selected cards fall on each distinction, and
   which ones they all agree on."
  [cards]
  (let [total  (count cards)
        common (game/shared cards)]
    [:div
     [:div {:style {:color faint :font-size "12px" :margin-bottom "12px" :line-height "18px"
                    :min-height "36px"}}
      (case total
        0 "click cards to see what they have in common"
        1 "one card selected"
        (if (seq common)
          [:span (str total " cards share ")
           [:span {:style {:color ink}} (count common)] ": "
           [:span {:style {:color ink}}
            (str/join " · " (map game/target-name common))]]
          (str total " cards share nothing")))]
     (for [[k base] legend-rows]
       ^{:key k}
       [:div {:style {:display "flex" :align-items "center" :gap "6px" :margin-bottom "4px"}}
        [:div {:style {:width "88px" :text-align "right" :color faint :font-size "12px"
                       :padding-right "6px"}}
         (name k)]
        (for [v [0 1]]
          ^{:key v}
          [legend-tile k v (if (= base :swatch) :swatch (sample base k v common))
           (count (filter #(= v (game/value % k)) cards)) total])])]))

;; ── Arranging your hand ────────────────────────────────────────────────────
;;
;; The order is yours alone: the server never hears about it. It is kept per
;; game in this browser so a reload does not shuffle what you arranged.

(defn- order-key [] (str "distinctions-order-" (when (exists? js/playKey) js/playKey)))

(defonce hand-order
  (r/atom (or (try (safe-read (.getItem js/localStorage (order-key)))
                   (catch :default _ nil))
              [])))

(defn- save-order! [order]
  (reset! hand-order order)
  (try (.setItem js/localStorage (order-key) (pr-str order))
       (catch :default _ nil)))

(defn- arranged
  "The hand in your order: the cards you have placed, then any new ones at
   the end, in the order they arrived."
  [hand]
  (let [kept (filterv (set hand) @hand-order)]
    (into kept (remove (set kept) hand))))

(defonce ^:private drag (r/atom nil))

(defn- toggle! [c] (swap! selected (fn [s] (if (s c) (disj s c) (conj s c)))))

(defn- card-under
  "The hand card at this point on the screen, if any."
  [x y]
  (some-> (js/document.elementFromPoint x y)
          (.closest "[data-card]")
          (.getAttribute "data-card")
          js/parseInt))

(defonce ^:private shown-order
  ;; the order last drawn, for the window-level drag handlers below; a plain
  ;; atom, since nothing should re-render because it changed
  (atom []))

(defn- drag-move! [e]
  (when-let [{:keys [card x y moved?]} @drag]
    (let [px (.-clientX e) py (.-clientY e)]
      (when (or moved? (> (js/Math.hypot (- px x) (- py y)) 6))
        (when-not moved? (swap! drag assoc :moved? true))
        (let [order @shown-order
              t     (card-under px py)]
          (when (and t (not= t card) (some #{t} order))
            (let [without (vec (remove #{card} order))
                  j       (.indexOf (clj->js order) t)]
              (save-order! (vec (concat (subvec without 0 j) [card] (subvec without j)))))))))))

(defn- drag-end! [_]
  (when-let [{:keys [card moved?]} @drag]
    (when-not moved? (toggle! card)))
  (reset! drag nil))

;; On the window, not the card: swapping moves the card's element, and a
;; moved element loses the pointer, so handlers on the card go quiet after
;; the first swap.
(defonce drag-listeners
  (do (.addEventListener js/window "pointermove" drag-move!)
      (.addEventListener js/window "pointerup" drag-end!)
      (.addEventListener js/window "pointercancel" #(reset! drag nil))
      true))

(defn- pointer-handlers
  "Press and release in place: select. Press and move: drag, swapping with
   each card the pointer passes over."
  [c]
  {:on-pointer-down (fn [e]
                      (.preventDefault e)
                      (reset! drag {:card c :x (.-clientX e) :y (.-clientY e) :moved? false}))})

(defn- your-hand [state]
  (let [you   (:you state)
        hand  (get-in state [:hands you])
        {:keys [discard]} (:actions state)
        ;; a card that has left the hand cannot stay selected
        picked (set (filter (set hand) @selected))
        one    (when (= 1 (count picked)) (first picked))]
    (when hand
      [:div {:style {:text-align "center"}}
       [:div {:style {:color faint :font-size "13px" :margin-bottom "10px" :min-height "34px"}}
        (cond
          (:winner state) ""
          discard [:div
                   [:div "your turn — select the card to throw away"]
                   [:div {:style {:margin-top "8px"}}
                    (if (contains? discard one)
                      [button "throw it" (fn []
                                           (send-action! {:action :discard :card one})
                                           (reset! selected #{}))
                       true]
                      [:span {:style {:font-size "12px"}}
                       (cond (= one (:taken state)) "you can't throw back what you just took"
                             one "" 
                             :else "(exactly one selected)")])]]
          (:actions state) "your turn — take the pile's top card, or draw from the deck"
          :else "")]
       [:div {:style {:display "flex" :gap "10px" :justify-content "center" :flex-wrap "wrap"}}
        (let [order (arranged hand)
              _     (reset! shown-order order)
              held  (:card @drag)
              moving? (:moved? @drag)]
          (for [c order]
            (let [on?      (contains? picked c)
                  dragged? (and moving? (= c held))]
              ^{:key c}
              [:div (merge
                     {:data-card c
                      :style {:transform (when on? "translateY(-12px)")
                              :transition "transform 120ms"
                              :border-radius "6px"
                              :opacity (if dragged? 0.55 1)
                              :cursor (if dragged? "grabbing" "pointer")
                              ;; no scrolling or text selection while dragging, on touch too
                              :touch-action "none" :user-select "none"
                              :box-shadow (when on? (str "0 0 0 3px " accent))}}
                     (pointer-handlers c))
               [card/card {:value c :width 84
                           :props {:style {:pointer-events "none"}}}]])))]])))

(defn- chat-view []
  (let [draft (r/atom "")]
    (fn []
      [:div {:style {:display "flex" :flex-direction "column" :gap "6px"}}
       [:div {:style {:height "140px" :overflow-y "auto" :background panel
                      :border-radius "6px" :padding "8px" :font-size "13px"}}
        (for [[i {:keys [player line]}] (map-indexed vector @chat-lines)]
          ^{:key i} [:div [:span {:style {:color faint}} player ": "] line])]
       [:input {:type "text" :value @draft :placeholder "say something"
                :on-change #(reset! draft (.. % -target -value))
                :on-key-down #(when (= "Enter" (.-key %))
                                (send-chat! @draft) (reset! draft ""))
                :style {:background ground :color ink :padding "8px"
                        :border "1px solid #34323a" :border-radius "4px"
                        :font-family "monospace"}}]])))

(defn- rail [state]
  [:div {:style {:width "280px" :flex-shrink 0 :padding "16px" :box-sizing "border-box"
                 :background "#18171b" :border-left "1px solid #2e2d33"
                 :height "100vh" :display "flex" :flex-direction "column" :gap "14px"}}
   [:div
    [:h2 {:style {:color ink :margin "0 0 6px 0" :letter-spacing "3px" :font-size "18px"}}
     "DISTINCTIONS"]
    [:div {:style {:color faint :font-size "12px" :line-height "18px"}}
     (str (:hand-size state) " cards · " goal-words)
     [:br] (str "turn " (:turn state))]]
   ;; the live key, where UNIVERSE keeps its hand history
   (let [hand (get-in state [:hands (:you state)])]
     [:div {:style {:flex "1 1 auto" :overflow-y "auto"}}
      (when hand [legend (filter (set @selected) hand)])])
   [:div [:div {:style {:color faint :font-size "12px" :margin-bottom "6px"}} "chat"]
    [chat-view]]])

(defn table-view []
  (let [state @game-state]
    [:div {:style {:background ground :min-height "100vh" :color ink
                   :font-family "monospace" :display "flex"}}
     [:div {:style {:flex "1 1 auto" :min-width 0 :padding "24px"}}
      (if (nil? state)
        [:div {:style {:color faint}} "connecting…"]
        (let [others (remove #(= (:seat %) (:you state)) (:players state))]
          [:div
           [:div {:style {:display "flex" :gap "12px" :justify-content "center" :flex-wrap "wrap"}}
            (for [p others] ^{:key (:seat p)} [opponent state p])]
           (cond
             (= :waiting (:phase state))
             [:div {:style {:text-align "center" :margin "60px 0"}}
              [:div {:style {:color faint :margin-bottom "16px"}}
               (str (:hand-size state) (if (solo? state) " cards." " cards each.")
                    " To win: " goal-words ".")]
              [button "deal" send-start! true]]

             :else
             [:div
              (when (:winner state)
                [:div {:style {:text-align "center" :margin-top "24px" :font-size "20px"}}
                 (if (solo? state)
                   [:span "done in " [:span {:style {:color red}} (:turn state)]
                    (if (= 1 (:turn state)) " turn" " turns")]
                   [:span [:span {:style {:color red}} (:winner state)] " wins"])
                 [:div {:style {:font-size "15px" :color faint :margin-top "6px"}}
                  (made-words (:made state))]
                 [:div {:style {:margin-top "14px"}}
                  [:a {:href "/distinctions/create" :style {:color faint :font-size "14px"}}
                   "new game"]]])
              [centre state]
              [your-hand state]])]))]
     (when state [rail state])]))

(defn create-view []
  [components/create-lobby
   {:game-type      "distinctions"
    :title          "DISTINCTIONS — New Game"
    :current-player (when (and (exists? js/playerKey) (not (str/blank? js/playerKey))
                               (not= "--observer--" js/playerKey))
                      js/playerKey)
    ;; one is a solo game; seven is as many hands of eight as the deck deals
    :min-players    1
    :max-players    7
    :accent         accent
    :slot-bg        panel
    :background     ground
    :aside          [:div {:style {:margin "-12px 0 24px 0" :font-size "13px"}}
                     [:a {:href "/distinctions/key.svg" :target "_blank"
                          :style {:color accent}} "the deck"]]}])

(defn observe-view []
  (let [games (safe-read (when (exists? js/observeGames) js/observeGames))]
    [:div {:style {:padding "32px" :background ground :color ink
                   :font-family "monospace" :min-height "100vh"}}
     [:h2 {:style {:color accent}} "DISTINCTIONS — games"]
     (if (seq games)
       (for [g games]
         ^{:key (:key g)}
         [:div {:style {:padding "10px 0" :border-bottom "1px solid #34323a"}}
          [:a {:href (str "/distinctions/play/" (:key g)) :style {:color accent}} (:key g)]
          [:span {:style {:color faint :margin-left "12px"}}
           (str (str/join ", " (:players g)) " · turn " (:turn g))]])
       [:div {:style {:color faint}} "no games running"])]))

(defn games-view []
  (let [games (safe-read (when (exists? js/playerGames) js/playerGames))]
    [:div {:style {:padding "32px" :background ground :color ink
                   :font-family "monospace" :min-height "100vh"}}
     [:h2 {:style {:color accent}} "your games"]
     (for [g (concat (:active games) (:complete games) (when (sequential? games) games))]
       ^{:key (:game g)}
       [:div {:style {:padding "8px 0"}}
        [:a {:href (str "/distinctions/play/" (:game g)) :style {:color accent}} (:game g)]
        [:span {:style {:color faint :margin-left "12px"}}
         (if (:winner g) (str (:winner g) " won") (str "turn " (:round g)))]])]))

(defn page []
  (cond
    (and (exists? js/isCreate) js/isCreate)   [create-view]
    (and (exists? js/isObserve) js/isObserve) [observe-view]
    (and (exists? js/isGames) js/isGames)     [games-view]
    (and (exists? js/playKey) (not (str/blank? js/playKey))) [table-view]
    :else [:div {:style {:padding "48px" :color faint :background ground
                         :font-family "monospace" :min-height "100vh"}}
           "Loading…"]))

(defn mount-components []
  (when-let [el (.getElementById js/document "distinctions")]
    (rdom/render [page] el))
  (let [pk (when (exists? js/playKey) js/playKey)]
    (when (and pk (not (str/blank? pk)) (not (and (exists? js/isCreate) js/isCreate)))
      (connect-ws! pk))))

(defn init! []
  (ajax/load-interceptors!)
  (mount-components))
