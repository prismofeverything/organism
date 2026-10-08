(ns universe.shed-table
  "The UNIVERSE shedding table: opponents across the top, the play to beat in
   the middle, your hand at the bottom. Select cards by clicking them; the
   line under your hand says what they make, and the play button lights only
   when they make something you may play.

   Messages go out through the functions play.cljs hands in, so this
   namespace never touches the socket itself."
  (:require
   [clojure.string :as str]
   [reagent.core :as r]
   [universe.card :as card]
   [universe.shed :as shed]))

(def ground "#12101c")
(def panel  "#1b1828")
(def gold   "#E6AD24")
(def faint  "#6b6480")
(def ink    "#d8d2e8")

(defonce selected (r/atom #{}))

(defn- toggle! [c] (swap! selected #(if (% c) (disj % c) (conj % c))))

(defn- button [label on-click enabled? & [primary?]]
  [:button {:on-click (when enabled? on-click)
            :disabled (not enabled?)
            :style {:background (cond (not enabled?) "#221d33" primary? gold :else "#2b2740")
                    :color (cond (not enabled?) faint primary? ground :else ink)
                    :border "none" :border-radius "6px" :padding "10px 22px"
                    :cursor (if enabled? "pointer" "default")
                    :font-family "monospace" :font-size "14px" :letter-spacing "1px"}}
   label])

(defn- name-of [state seat] (shed/player-name state seat))

(defn- opponent [state {:keys [seat name]}]
  (let [acting? (= seat (:to-act state))
        passed? (contains? (:passed state) seat)
        n       (get-in state [:hand-counts seat] 0)
        shown   (get-in state [:hands seat])
        winner? (= name (:winner state))]
    [:div {:style {:background (if acting? "#3a2f10" panel)
                   :border (str "1px solid " (if (or acting? winner?) gold "#322b48"))
                   :border-radius "8px" :padding "10px 14px" :min-width "130px"}}
     [:div {:style {:color (if acting? gold ink) :font-size "14px"}} name]
     [:div {:style {:color faint :font-size "12px" :margin-top "4px"}}
      (cond winner? "out — wins"
            acting? (if (:lead state) "to beat it…" "leading…")
            passed? "passed"
            :else   (str n (if (= 1 n) " card" " cards")))]
     (when shown
       [:div {:style {:display "flex" :gap "2px" :margin-top "6px" :flex-wrap "wrap"}}
        (for [c (sort-by shed/rank shown)]
          ^{:key c} [card/card {:value c :width 22}])])]))

(defn- the-lead [state]
  (let [{:keys [cards seat]} (:lead state)]
    [:div {:style {:text-align "center" :margin "30px 0" :min-height "150px"}}
     (if cards
       [:div
        [:div {:style {:display "flex" :gap "8px" :justify-content "center"}}
         (for [c (sort-by shed/rank cards)]
           ^{:key c} [card/card {:value c :width 74}])]
        [:div {:style {:color faint :font-size "13px" :margin-top "10px"}}
         [:span {:style {:color ink}} (name-of state seat)] " · " (shed/describe-play cards)]]
       [:div {:style {:color faint :padding-top "50px"}}
        (when-let [who (shed/current-player state)]
          (str who " leads — anything"))])]))

(defn- your-hand [state send-action!]
  (let [you    (:you state)
        hand   (sort-by shed/rank (get-in state [:hands you]))
        picked (vec (filter (set @selected) hand))
        p      (shed/classify picked)
        lead   (:lead state)
        {:keys [pass] :as actions} (:actions state)
        ok?    (and actions p (or (nil? lead) (shed/beats? p lead)))]
    (when (seq hand)
      [:div {:style {:text-align "center"}}
       [:div {:style {:display "flex" :gap "8px" :justify-content "center" :flex-wrap "wrap"}}
        (for [c hand]
          (let [on? (contains? (set picked) c)]
            ^{:key c}
            [:div {:on-click #(toggle! c)
                   :style {:cursor "pointer" :border-radius "6px"
                           :transform (when on? "translateY(-12px)")
                           :transition "transform 120ms"
                           :box-shadow (when on? (str "0 0 0 3px " gold))}}
             [card/card {:value c :width 70}]]))]
       [:div {:style {:color faint :font-size "13px" :margin "14px 0 10px" :min-height "18px"}}
        (cond
          (empty? picked) (if actions "select cards to play" "")
          (nil? p)        "those cards are not a play"
          (and lead (not (shed/beats? p lead)))
          (str (shed/describe-play picked) " — does not beat it")
          :else           (shed/describe-play picked))]
       (when actions
         [:div {:style {:display "flex" :gap "10px" :justify-content "center"}}
          [button "play" (fn []
                           (send-action! {:action :play :cards picked})
                           (reset! selected #{}))
           ok? true]
          (when lead
            [button "pass" #(do (send-action! {:action :pass}) (reset! selected #{})) pass])])])))

(defn- log-line [state {:keys [event seat cards]}]
  (let [who (name-of state seat)]
    (case event
      :deal  (str who " holds the lowest card and leads")
      :play  (str who ": " (shed/describe-play cards))
      :pass  (str who " passes")
      :trick (str who " takes it")
      :out   (str who " is out")
      nil)))

(defn- rail [state chat]
  [:div {:style {:width "300px" :flex-shrink 0 :padding "16px" :box-sizing "border-box"
                 :background "#15121f" :border-left "1px solid #2b2740"
                 :height "100vh" :display "flex" :flex-direction "column" :gap "14px"}}
   [:div
    [:h2 {:style {:color gold :margin "0 0 6px 0" :letter-spacing "3px" :font-size "20px"}}
     "UNIVERSE"]
    [:div {:style {:color faint :font-size "12px" :line-height "18px"}}
     "shedding · first out wins" [:br]
     "sets of 2–4, runs of 3–5, one color or one shape" [:br]
     "beat: same kind, same size, higher top card"]]
   [:div {:style {:flex "1 1 auto" :overflow-y "auto" :background panel :border-radius "6px"
                  :padding "8px" :font-size "12px" :line-height "18px"
                  :display "flex" :flex-direction "column-reverse"}}
    [:div
     (for [[i line] (map-indexed vector (keep #(log-line state %) (:log state)))]
       ^{:key i} [:div {:style {:color (if (str/includes? line "takes it") gold "#b6afc9")}}
                  line])]]
   [:div [:div {:style {:color faint :font-size "12px" :margin-bottom "6px"}} "chat"]
    chat]])

(defn table
  "The whole shedding table. `send-action!`, `send-start!` and the `chat`
   component come from play.cljs."
  [state {:keys [send-action! send-start! chat]}]
  [:div {:style {:background ground :min-height "100vh" :color ink
                 :font-family "monospace" :display "flex"}}
   [:div {:style {:flex "1 1 auto" :min-width 0 :padding "24px"}}
    [:div {:style {:display "flex" :gap "12px" :justify-content "center" :flex-wrap "wrap"}}
     (for [p (:players state) :when (not= (:seat p) (:you state))]
       ^{:key (:seat p)} [opponent state p])]
    (cond
      (= :waiting (:phase state))
      [:div {:style {:text-align "center" :margin "70px 0"}}
       [:div {:style {:color faint :margin-bottom "16px"}}
        (str (shed/hand-size (count (:players state))) " cards each. First out wins.")]
       [button "deal" send-start! true true]]

      :else
      [:div
       (when (:winner state)
         [:div {:style {:text-align "center" :margin-top "20px" :font-size "22px" :color gold}}
          (:winner state) " is out — and wins"
          [:div {:style {:margin-top "12px" :font-size "14px"}}
           [:a {:href "/universe/create" :style {:color faint}} "new table"]]])
       [the-lead state]
       [your-hand state send-action!]])]
   [rail state chat]])
