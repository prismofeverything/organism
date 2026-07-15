(ns journey.play
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [journey.game :as game]
   [journey.choice :as choice]
   [journey.board :as board]
   [organism.ajax :as ajax]
   [organism.components :as components]
   [organism.websockets :as ws]))

;; ── State ─────────────────────────────────────────────────────────────────────

(defonce game-state
  (r/atom nil))

(defonce player-key
  (r/atom (when (exists? js/playerKey) js/playerKey)))

(defonce player-preferences
  (r/atom {}))

(defonce observe-games
  (r/atom []))

(defonce bots-set
  (r/atom #{}))

(defonce can-undo?
  (r/atom false))

;; Server-provided choices: the server computes find-state-raw and sends the choice keys.
;; This avoids stale .cljc auto-advance issues.
(defonce server-choices
  (r/atom nil))

(defonce play-history
  (r/atom []))

;; nil = viewing live (latest), number = viewing historical step
(defonce history-view-step
  (r/atom nil))

;; History/log panel visibility — hidden by default, toggled with ESC.
(defonce history-visible?
  (r/atom false))

;; When non-nil, the player has clicked a convert ghost station and must pick
;; which sundiver arrangement to use. Value: {:target pos :type t :options [conv...]}
(defonce pending-convert
  (r/atom nil))

(defonce cipher-expanded?
  (r/atom false))

;; {:pos cipher-pos :color color-key} when hovering a beacon on cipher
(defonce cipher-hover
  (r/atom nil))

;; When true, game-over info shows in the status oval instead of center banner
(defonce game-over-collapsed?
  (r/atom false))

;; Game-over landing animation state
;; {:phase :zoom|:reveal|:done, :step 0, :revealed #{cipher-pos ...}, :landing-pos [q r]}
(defonce landing-anim (r/atom nil))
;; Track which game-over we've already animated (play-key or state identity)
(defonce landing-anim-triggered (r/atom nil))

;; ── Helpers ───────────────────────────────────────────────────────────────────

(defn choice-player
  "The player who must make the current choice (may differ from current-player
   for phases like :choose-activate-owner-bonus)."
  [state]
  (or (get-in state [:player-turn :choice-player])
      (game/current-player state)))

(defn my-turn?
  "True if the local player is the one who must choose right now."
  [state my-player]
  (and (some? state)
       (= (choice-player state) my-player)
       (not (:game-over state))))

;; ── Choice → UI mapping ───────────────────────────────────────────────────────

(defn hex-pos? [k]
  (and (vector? k) (= 2 (count k)) (number? (first k))))

(defn wrap-choice? [k]
  (and (vector? k) (= 2 (count k)) (= :wrap (first k))))

(defn fly-from? [k]
  (and (vector? k) (= 2 (count k)) (= :fly (first k))))

(defn choice-label
  "Human-readable label for a choice key."
  [k]
  (cond
    (nil? k)           "habitat"
    (= k :none)        "straight"
    (keyword? k)       (name k)
    (and (integer? k) (zero? k)) "decline bonus"
    (integer? k)       (str "take bonus " k)
    (hex-pos? k)       (str "[" (first k) "," (second k) "]")
    (wrap-choice? k)   (str "wrap → " (choice-label (second k)))
    (fly-from? k)      (str "fly from " (choice-label (second k)))
    (and (map? k) (:type k) (:target k))
    (str (name (:type k)) " @ " (choice-label (:target k)))
    (and (map? k) (:suit k))
    (str (get game/suit-names (:suit k) "?"))
    :else              (pr-str k)))

(defn convert-choice? [k]
  (and (map? k) (contains? k :type) (contains? k :target)))

(defn card-choice? [k]
  (and (map? k) (contains? k :suit)))

(defn partition-choices
  "Split choices into pos-highlights, fly-from highlights, wrap positions,
   convert info, card choices, habitat-pay choice, and buttons.
   Returns [pos-set fly-from-set wrap-set conv-groups card-choices habitat-choice button-choices]."
  [choices]
  (reduce-kv
   (fn [[pos-set fly-set wrap-set conv-groups cards hab btns] k _v]
     (cond
       (hex-pos? k)       [(conj pos-set k) fly-set wrap-set conv-groups cards hab btns]
       (fly-from? k)      [pos-set (conj fly-set (second k)) wrap-set conv-groups cards hab btns]
       (wrap-choice? k)   [pos-set fly-set (conj wrap-set (second k)) conv-groups cards hab btns]
       (convert-choice? k)
       [pos-set fly-set wrap-set
        (update conv-groups [(:target k) (:type k)]
                (fnil conj []) {:choice-key k :sundivers (:sundivers k)})
        cards hab btns]
       (nil? k)           [pos-set fly-set wrap-set conv-groups cards true btns]
       (card-choice? k)   [pos-set fly-set wrap-set conv-groups (conj cards {:card k :choice-key k}) hab btns]
       :else [pos-set fly-set wrap-set conv-groups cards hab (conj btns {:label (choice-label k) :choice-key k})]))
   [#{} #{} #{} {} [] nil []]
   choices))

;; ── WebSocket communication ───────────────────────────────────────────────────

(defn send-action!
  "Send a choice to the server."
  [choice-key]
  (ws/send-transit-message! {:type "action" :choice (pr-str choice-key)}))

(defn send-undo!
  "Ask the server to undo the last action."
  []
  (ws/send-transit-message! {:type "undo"}))

;; ── Choice interaction ────────────────────────────────────────────────────────

(defn on-hex-click [state my-player choices pos]
  (when (my-turn? state my-player)
    (when-let [_ (get choices pos)]
      (send-action! pos))))

(defn on-button-click [choice-key]
  (send-action! choice-key))

;; ── Board pan/zoom (shared so landing animation can drive them) ─────────────

(defonce board-pan-x (r/atom 0))
(defonce board-pan-y (r/atom 0))
(defonce board-zoom  (r/atom 1.0))

;; ── Landing animation ───────────────────────────────────────────────────────

(defn compute-landing-matches
  "Return a seq of cipher positions (center + directions) that match at landing-pos."
  [state landing-pos]
  (let [board      (:board state)
        tile-color (get-in board [landing-pos :color])
        center     (when (game/cipher-color-active? state [0 0] tile-color) [[0 0]])
        dirs       (keep (fn [dir]
                           (let [neighbor  (game/add-hex landing-pos dir)
                                 n-color   (get-in board [neighbor :color])]
                             (when (and n-color (game/cipher-color-active? state dir n-color))
                               dir)))
                         game/hex-directions)]
    (vec (concat center dirs))))

(defn start-landing-animation!
  "Kick off the zoom → reveal → done sequence for a landing game-over."
  [state]
  (let [landing-pos (get-in state [:game-over :tile])
        matches     (compute-landing-matches state landing-pos)
        [tx ty]     (board/hex->pixel landing-pos)
        [bcx bcy]   (board/board-centroid (:board state))
        z           2.0]
    ;; Phase 1: zoom to landing tile
    ;; Transform: translate(780+panX, 450+panY) scale(z) translate(-bcx,-bcy)
    ;; To center tile (tx,ty): panX = -z*(tx-bcx), panY = -z*(ty-bcy)
    (reset! landing-anim {:phase :zoom :step 0 :revealed #{} :matches matches
                          :landing-pos landing-pos})
    (reset! board-zoom z)
    (reset! board-pan-x (- (* z (- tx bcx))))
    (reset! board-pan-y (- (* z (- ty bcy))))
    ;; Phase 2: after zoom settles, start revealing beacons one by one
    (js/setTimeout
     (fn []
       (let [ms (volatile! 0)]
         (doseq [[i m] (map-indexed vector matches)]
           (js/setTimeout
            (fn []
              (swap! landing-anim
                     (fn [a] (-> a
                                 (update :revealed conj m)
                                 (assoc :step (inc i))))))
            @ms)
           (vswap! ms + 600))
         ;; Phase 3: done
         (js/setTimeout
          (fn [] (swap! landing-anim assoc :phase :done))
          (+ @ms 400))))
     800)))

;; ── Interactive board view (pan / zoom) ──────────────────────────────────────

(defn board-view
  "Wraps render-game with mouse-drag pan and keyboard/slider zoom.
   Arrow keys pan; PageUp/PageDown zoom; drag the starfield to pan.
   Optional: on-navigate, fly-highlights, chosen-pos."
  [state pos-highlights on-hex-click choice-buttons & [{:keys [on-navigate fly-highlights chosen-pos active-player conv-groups conv-sundivers pending-convert cipher-highlights cipher-on-click cipher-on-beacon-hover cipher-hover cipher-queue-color on-habitat-click habitat-player]}]]
  (r/with-let
    [drag  (r/atom nil)
     on-key
     (fn [e]
       (case (.-key e)
         "ArrowLeft"  (swap! board-pan-x + 60)
         "ArrowRight" (swap! board-pan-x - 60)
         "ArrowUp"    (if on-navigate (on-navigate :up)   (swap! board-pan-y + 60))
         "ArrowDown"  (if on-navigate (on-navigate :down) (swap! board-pan-y - 60))
         "PageUp"     (swap! board-zoom #(min 4.0 (* % 1.15)))
         "PageDown"   (swap! board-zoom #(max 0.2  (/ % 1.15)))
         " "          (do (reset! board-pan-x 0) (reset! board-pan-y 0) (reset! board-zoom 1.0))
         "Escape"     (swap! history-visible? not)
         nil))
     on-move
     (fn [e]
       (when-let [{:keys [mx my px py]} @drag]
         (reset! board-pan-x (+ px (- (.-clientX e) mx)))
         (reset! board-pan-y (+ py (- (.-clientY e) my)))))
     on-up (fn [_] (reset! drag nil))
     _     (do (js/document.addEventListener "keydown" on-key)
               (js/document.addEventListener "mousemove" on-move)
               (js/document.addEventListener "mouseup" on-up))]
    [:div {:style {:position "relative" :width "100%" :height "100%"}}
     [board/render-game state pos-highlights on-hex-click choice-buttons
      {:pan-x @board-pan-x :pan-y @board-pan-y :zoom @board-zoom
       :fly-highlights fly-highlights
       :chosen-pos chosen-pos
       :active-player active-player
       :conv-groups conv-groups
       :conv-sundivers conv-sundivers
       :pending-convert pending-convert
       :cipher-highlights cipher-highlights
       :cipher-on-click cipher-on-click
       :cipher-on-beacon-hover cipher-on-beacon-hover
       :cipher-hover cipher-hover
       :cipher-queue-color cipher-queue-color
       :landing-revealed (:revealed @landing-anim)
       :on-habitat-click on-habitat-click
       :habitat-player habitat-player
       :on-bg-mouse-down
       (fn [e]
         (.preventDefault e)
         (reset! drag {:mx (.-clientX e) :my (.-clientY e)
                       :px @board-pan-x :py @board-pan-y}))}]
     [:div {:style {:position "absolute" :bottom "16px" :left "16px"
                    :display "flex" :align-items "center" :gap "8px"}}
      [:input {:type "range" :min 20 :max 400 :step 5
               :value (int (* @board-zoom 100))
               :on-change #(reset! board-zoom (/ (js/parseInt (.. % -target -value)) 100))
               :style {:width "100px" :cursor "pointer"}}]
      [:span {:style {:color "#334455" :font-family "monospace" :font-size "10px"
                      :min-width "32px"}}
       (str (int (* @board-zoom 100)) "%")]]]
    (finally
      (js/document.removeEventListener "keydown" on-key)
      (js/document.removeEventListener "mousemove" on-move)
      (js/document.removeEventListener "mouseup" on-up))))

;; ── Shared history row component ─────────────────────────────────────────────
;; Used by both play-page and generate-page.

(def history-row
  (r/create-class
   {:display-name "history-row"
    :should-component-update
    (fn [_ [_ _ _ _ old-sel?] [_ _ _ _ new-sel?]]
      (not= old-sel? new-sel?))
    :reagent-render
    (fn [i {:keys [step player phase choice]} player-colors selected? group-start? show-sep? on-click]
      (let [ck   (get player-colors player :sun)
            fc   (board/pwi ck)
            bg-c (board/ptb ck)]
        [:div
         (when show-sep?
           [:div {:style {:height "1px" :margin "3px 0"
                          :background bg-c :opacity 0.4}}])
         (when group-start?
           [:div {:style {:padding "4px 10px 1px"
                          :color fc
                          :font-size "12px" :font-family "monospace"
                          :letter-spacing "1px"
                          :background bg-c :opacity (if selected? 1.0 0.7)}}
            (str "▸ " (or player "—"))])
         [:div
          {:id       (str "hist-item-" i)
           :on-click on-click
           :style {:padding "4px 10px 4px 14px" :cursor "pointer"
                   :border-left (str "2px solid " bg-c)
                   :background bg-c
                   :opacity (if selected? 0.9 0.5)}}
          [:div {:style {:color fc
                         :font-size "15px" :font-family "monospace"}}
           (str "·" (or step i) "  " (name (or phase :?)))]
          [:div {:style {:color fc
                         :font-size "13px" :font-family "monospace"
                         :word-break "break-all"
                         :opacity 0.8}}
           choice]]]))}))

;; ── Play page ─────────────────────────────────────────────────────────────────

(defn game-phase-label [state]
  (let [phase (game/current-phase state)
        cp    (choice-player state)]
    (str (name (or phase :?)) " — " cp)))

(defn play-page []
  (let [live-state @game-state
        my         @player-key
        bots       @bots-set
        undo?      @can-undo?
        history    @play-history
        view-step  @history-view-step
        n          (count history)
        ;; If viewing a historical step, use that state; otherwise live
        viewing-history? (and @history-visible? view-step (< view-step (dec n)))
        state      (if (and viewing-history? (< view-step n))
                     (:state (nth history view-step))
                     live-state)]
    (if (nil? state)
      [:div {:style {:color "#556" :padding "40px" :font-family "monospace"}}
       [:p "Waiting for game state…"]]
      (let [;; Use ONLY server-provided choice keys. No local find-state computation.
            ;; The server is authoritative for what choices exist.
            srv         @server-choices
            choices     (if srv
                          (into {} (map (fn [k] [k state]) (:keys srv)))
                          {})
            active?           (and (not viewing-history?) (my-turn? state my))
            cp                (choice-player state)
            bot-thinking?     (and (not active?) (contains? bots cp) (not (:game-over state)))
            ;; Detect cipher phase: cipher positions go to cipher display, not board
            is-cipher?        (= :cipher (:phase srv))
            cipher-positions  (when is-cipher?
                                (set (filter hex-pos? (keys choices))))
            ;; Remove cipher positions from choices before partition (they go to cipher display)
            board-choices     (if is-cipher?
                                (into {} (remove #(hex-pos? (key %)) choices))
                                choices)
            [pos-hl fly-hl wrap-hl conv-groups card-choices habitat-choice btn-choices]
            (if active?
              (partition-choices board-choices)
              [#{} #{} #{} {} [] nil []])
            pending         @pending-convert
            ;; Compute actual wrap destination positions and map them to choice keys.
            ;; Origin depends on context: ark for launch wraps, fly-from for fly wraps.
            ark             (:ark state)
            fly-from        (get-in state [:player-turn :action :fly-from])
            wrap-origin     (or fly-from ark)
            wrap-dest->key  (when wrap-origin
                              (into {}
                                (for [edge-pos wrap-hl
                                      :let [dir      (mapv - edge-pos wrap-origin)
                                            dest-pos (game/wrap-target state wrap-origin dir)]
                                      :when (and dest-pos
                                                 (some #(= % dir) game/hex-directions))]
                                  [dest-pos [:wrap edge-pos]])))
            wrap-dest-set   (set (keys (or wrap-dest->key {})))
            ;; Convert sundiver highlights depend on pending selection
            conv-sundivers  (if pending
                              (set (mapcat :sundivers (:options pending)))
                              (set (mapcat (fn [[_ opts]] (mapcat :sundivers opts)) conv-groups)))
            ;; Merge all highlights (cipher hover matches computed in render-game)
            all-pos-hl      (into pos-hl wrap-dest-set)
            on-click        (when active?
                              (fn [pos-or-key]
                                (cond
                                  ;; Convert choice-key map from pending sundiver selection
                                  (map? pos-or-key)
                                  (do (reset! pending-convert nil)
                                      (send-action! pos-or-key))
                                  ;; Convert ghost station: [target type] where target is a vector
                                  (and (vector? pos-or-key) (= 2 (count pos-or-key))
                                       (vector? (first pos-or-key)) (keyword? (second pos-or-key)))
                                  (let [opts (get conv-groups pos-or-key)]
                                    (if (= 1 (count opts))
                                      (do (reset! pending-convert nil)
                                          (send-action! (:choice-key (first opts))))
                                      (reset! pending-convert
                                              {:target (first pos-or-key) :type (second pos-or-key)
                                               :options opts})))
                                  ;; [:fly pos] from sundiver click
                                  (and (vector? pos-or-key) (= :fly (first pos-or-key)))
                                  (send-action! pos-or-key)
                                  (contains? pos-hl pos-or-key)          (send-action! pos-or-key)
                                  (and wrap-dest->key (contains? wrap-dest->key pos-or-key))
                                  (send-action! (get wrap-dest->key pos-or-key))
                                  :else                                  (send-action! pos-or-key))))
            chosen-pos        (get-in state [:player-turn :action :fly-from])
            undo-btn          (when (and active? undo?)
                                [{:label "undo" :on-click send-undo!}])
            filtered-btns     btn-choices
            buttons           (concat
                               undo-btn
                               (when active?
                                 (map (fn [{:keys [label choice-key]}]
                                        {:label    label
                                         :on-click #(on-button-click choice-key)})
                                      filtered-btns)))
            player-order      (:turn-order state)
            player-colors     (board/build-player-colors player-order)
            n                 (count history)]
        [:div {:style {:display "flex" :width "100vw" :height "100vh"
                       :background "#04040E" :overflow "hidden"}}
         ;; Board area
         [:div {:style {:flex "1" :position "relative" :overflow "hidden"}}
          ;; Phase/turn indicator strip
          (let [srv-phase   (:phase srv)
                cp-name     (choice-player state)
                p-colors    (board/build-player-colors (:turn-order state))
                cp-ck       (get p-colors cp-name :sun)
                cp-fg       (board/pwi cp-ck)
                cp-bg       (board/ptb cp-ck)
                cp-border   (board/pwo cp-ck)
                action-type (get-in state [:player-turn :action-type])
                station-type (get-in state [:player-turn :action :station-type])
                summary
                (case srv-phase
                  :choose-action-type  "choose an action: move, convert, or activate"
                  :choose-deconvert    "no usable sundivers — deconvert a station to reclaim them"
                  :choose-move         (let [rem (get-in state [:player-turn :action :moves-remaining] 0)
                                             tot (game/move-points state (game/current-player state))]
                                         (str "move sundivers (" rem "/" tot " remaining)"))
                  :choose-fly-to       "choose where to fly"
                  :choose-convert      "choose a conversion pattern"
                  :choose-activate     "choose a station to activate"
                  :choose-activate-station "choose a station to activate"
                  :choose-activate-self-bonus "choose how many bonus actions to take"
                  :choose-activate-owner-bonus "choose how many owner bonus actions to take"
                  :choose-activate-matrix-beacon "place a beacon"
                  :choose-activate-matrix-spend "spend a sundiver to pay for the beacon"
                  :choose-activate-tower-heading "choose the Ark's heading"
                  :choose-activate-tower-join "join a beacon to the cipher?"
                  :choose-activate-tower-join-spend "spend a sundiver to join"
                  :choose-activate-tower-spend "spend a sundiver to pay for the tower action"
                  :choose-captain-drift "choose the Ark's drift direction"
                  :choose-ark-advance   "the Ark reaches unexplored space"
                  :choose-flare-advance "a flare advances the Ark into unexplored space"
                  :choose-drift-flare-advance "drift flare advances the Ark"
                  :choose-land          "land the Ark?"
                  :cipher               (let [entry (first (get-in state [:player-turn :cipher-queue] []))
                                              cname (when entry (name (:color entry)))]
                                          (if cname
                                            (str "place a " cname " beacon on the cipher")
                                            "place a beacon on the cipher"))
                  :cipher-spend         (let [entry (first (get-in state [:player-turn :cipher-queue] []))
                                              cname (when entry (name (:color entry)))]
                                          (if cname
                                            (str "spend a sundiver to place " cname " beacon")
                                            "spend a sundiver to pay for cipher placement"))
                  :keep-card            "choose a card to keep"
                  :flare-beacon-join    "join a beacon to the cipher?"
                  :flare-beacon-join-spend "spend a sundiver to join"
                  :captain-beacon-join  "join a beacon to the cipher?"
                  :captain-beacon-join-spend "spend a sundiver to join"
                  :draw-cards           "drawing cards…"
                  :draw-drift-card      "drawing drift card…"
                  :game-over            (let [go (:game-over state)]
                                          (if (= :landing (:type go))
                                            (str "landed! "
                                                 (str/join ", "
                                                   (map (fn [[p sc]] (str p ": " sc))
                                                        (:scores go))))
                                            (str "loss — flares " (:flares-drawn state 0) "/13")))
                  (when srv-phase (name srv-phase)))]
            [:div {:style {:position "absolute" :top "-30px" :left "50%"
                           :transform "translateX(-50%)"
                           :color "#AABBCC" :font-size "21px"
                           :font-family "monospace" :z-index 10
                           :background cp-bg
                           :border (str "2px solid " cp-border)
                           :border-top "none"
                           :border-radius "50%"
                           :padding "40px 48px 20px"
                           :text-align "center"
                           :display "flex" :flex-direction "column"
                           :align-items "center" :gap "10px"}}
             ;; Top line: player name + context + summary
             [:div {:style {:white-space "nowrap"}}
              [:span {:style {:color cp-fg :font-weight "bold" :font-size "24px"}} cp-name]
              (when action-type
                [:span {:style {:color cp-fg :opacity 0.55 :margin-left "10px" :font-size "20px"}}
                 (str "(" (name action-type)
                      (when station-type (str " " (name station-type)))
                      ")")])
              (when summary
                [:span {:style {:color cp-fg :opacity 0.7 :margin-left "12px" :font-size "20px"}} summary])
              (when active?
                [:span {:style {:color "#88CCAA" :margin-left "12px" :font-size "20px" :font-weight "bold"}} "← your turn"])]
             ;; Choice buttons row
             (when (seq buttons)
               [:div {:style {:display "flex" :gap "8px" :flex-wrap "wrap"
                              :justify-content "center"}}
                (for [[i {:keys [label on-click]}] (map-indexed vector buttons)]
                  [:button {:key i :on-click on-click
                            :style {:background cp-bg
                                    :color cp-fg
                                    :border (str "2px solid " cp-border)
                                    :border-radius "5px"
                                    :padding "6px 20px"
                                    :cursor "pointer"
                                    :font-family "monospace"
                                    :font-size "20px"
                                    :font-weight "bold"}}
                   label])])
             ;; Card choice row (keep-card phase)
             (when (seq card-choices)
               [:div {:style {:display "flex" :gap "12px" :flex-wrap "wrap"
                              :justify-content "center"}}
                ;; All keepable cards (held + drawn) shown as equal options.
                (for [[i {:keys [card choice-key]}] (map-indexed vector card-choices)
                      :let [suit (:suit card)
                            sc   (get game/suit-colors suit "#555")
                            sn   (get game/suit-names suit "?")]]
                  [:div {:key i
                         :on-click #(send-action! choice-key)
                         :style {:cursor "pointer" :text-align "center"}}
                   [:div {:style {:width "44px" :height "64px" :border-radius "5px"
                                  :background sc :border "2px solid #FFFFFF33"
                                  :display "flex" :align-items "center" :justify-content "center"}}
                    [:svg {:width "28" :height "28" :viewBox "-10 -10 20 20"}
                     [board/suit-icon suit "#FFF" 8]]]
                   [:div {:style {:color sc :font-size "13px" :margin-top "2px"
                                  :font-family "monospace"}} sn]])])])
          ;; Game-over banner (click to collapse into status oval)
          (when (and (:game-over state) (not @game-over-collapsed?))
            (let [go (:game-over state)]
              [:div {:style {:position "absolute"
                             :top "40%" :left "50%"
                             :transform "translate(-50%,-50%)"
                             :background "#0A0E1C"
                             :border "2px solid #3A5090"
                             :border-radius "8px"
                             :padding "32px 48px"
                             :color "#AAC8EE"
                             :font-family "monospace"
                             :font-size "24px"
                             :z-index 20
                             :text-align "center"
                             :cursor "pointer"}
                     :on-click #(reset! game-over-collapsed? true)}
               [:div {:style {:font-size "29px" :margin-bottom "12px"}} "GAME OVER"]
               (if (= :landing (:type go))
                 [:div
                  [:div (str "Landing at " (pr-str (:tile go)))]
                  (for [[p sc] (:scores go)]
                    [:div {:key p :style {:margin-top "4px"}}
                     (str p ": " sc " pts")])]
                 [:div
                  [:div (str "Loss — captain: " (:captain go))]
                  [:div {:style {:margin-top "8px" :color "#889"}}
                   (str "Flares: " (:flares-drawn state 0) "/13")]])]))
          ;; SVG board
          [board-view state all-pos-hl on-click nil
           {:fly-highlights fly-hl :chosen-pos chosen-pos :active-player my
            :conv-groups conv-groups :conv-sundivers conv-sundivers
            :pending-convert pending
            :cipher-highlights (when active? cipher-positions)
            :cipher-on-click (when (and active? is-cipher?)
                               (fn [pos] (send-action! pos)))
            :cipher-on-beacon-hover (fn [pos color]
                                      (reset! cipher-hover
                                              (when pos {:pos pos :color color})))
            :cipher-hover @cipher-hover
            :cipher-queue-color (when is-cipher?
                                  (:color (first (get-in state [:player-turn :cipher-queue] []))))
            :on-habitat-click (when habitat-choice
                                (fn [] (send-action! nil)))
            :habitat-player (when habitat-choice my)}]]
         ;; History panel (right side) — hidden by default, toggled with ESC.
         (when @history-visible?
           (let [sel       (or view-step (dec n))
               btn-style {:background "none" :border "1px solid #1E2A3A"
                          :border-radius "3px" :color "#556677"
                          :font-size "11px" :cursor "pointer"
                          :padding "2px 6px" :font-family "monospace"}]
           [:div {:style {:width "250px" :display "flex" :flex-direction "column"
                          :background "#05060F" :border-left "1px solid #141830"}}
            ;; Header with step counter
            [:div {:style {:padding "4px 10px 3px"
                           :border-bottom "1px solid #141830"
                           :display "flex" :align-items "center"}}
             [:span {:style {:flex "1" :color "#334455" :font-size "9px"
                             :font-family "monospace" :letter-spacing "1.5px"
                             :text-transform "uppercase"}}
              (str "step " sel " / " (dec n))]
             (when viewing-history?
               [:span {:style {:color "#886644" :font-size "9px" :font-family "monospace"}}
                "viewing history"])]
            ;; Playback controls
            [:div {:style {:display "flex" :justify-content "center" :gap "4px"
                           :padding "4px 8px" :border-bottom "1px solid #141830"}}
             ;; Rewind to beginning
             [:button {:style btn-style
                       :on-click #(reset! history-view-step 0)}
              "⏮"]
             ;; Back one step
             [:button {:style btn-style
                       :on-click #(swap! history-view-step
                                         (fn [s] (max 0 (dec (or s (dec n))))))}
              "◀"]
             ;; Play/pause (jump to live)
             [:button {:style (merge btn-style
                                     (when-not viewing-history?
                                       {:color "#88CCAA" :border-color "#2A4A3A"}))
                       :on-click #(reset! history-view-step nil)}
              (if viewing-history? "▶ live" "● live")]
             ;; Forward one step
             [:button {:style btn-style
                       :on-click #(swap! history-view-step
                                         (fn [s] (let [ns (inc (or s (dec n)))]
                                                   (if (>= ns (dec n)) nil ns))))}
              "▶"]
             ;; Jump to latest
             [:button {:style btn-style
                       :on-click #(reset! history-view-step nil)}
              "⏭"]]
            ;; Scrollable step list
            (do
              (r/after-render
               #(when-let [el (.getElementById js/document (str "hist-item-" sel))]
                  (.scrollIntoView el #js {:block "nearest" :behavior "instant"})))
              [:div {:style {:flex "1" :overflow-y "auto" :padding "4px 0"}}
               (for [i (range (dec n) -1 -1)]
                 (let [{:keys [player phase] :as entry} (nth history i)
                       above-player (:player (when (< i (dec n)) (nth history (inc i))))
                       group-start? (not= player above-player)
                       show-sep?    (and group-start? (< i (dec n)))
                       selected?    (= i sel)]
                   ^{:key i}
                   [history-row i entry player-colors selected? group-start? show-sep?
                    (fn []
                      (reset! history-view-step i)
                      ;; If this entry has no state (pre-reload), request it from server
                      (when-not (:state (nth history i))
                        (ws/send-transit-message!
                         {:type "replay-state" :step i})))]))])]))
         ]
        ))))


;; ── Create / Observe pages ────────────────────────────────────────────────────

(def input-style
  {:background "#0A0E1C" :color "#AACCEE"
   :border "1px solid #2A4A80" :border-radius "4px"
   :padding "8px 12px" :font-family "monospace" :font-size "14px"})

(def btn-style
  {:background "#10182A" :color "#7AAAE0"
   :border "1px solid #2A4A80" :border-radius "4px"
   :padding "8px 20px" :cursor "pointer"
   :font-family "monospace" :font-size "14px"})

(defn create-page []
  [components/create-lobby
   {:game-type      "journey"
    :title          "JOURNEY — New Game"
    :current-player (when (and (exists? js/playerKey)
                               (not (str/blank? js/playerKey)))
                      js/playerKey)
    :min-players    1
    :max-players    5
    :accent         "#7AAAE0"
    :slot-bg        "#10182A"
    :background     "#04040E"}])

(defn observe-page []
  (let [games @observe-games]
    [:div {:style {:color "#AABBCC" :padding "48px"
                   :font-family "monospace" :background "#04040E"
                   :min-height "100vh"}}
     [:div {:style {:display "flex" :align-items "center" :margin-bottom "32px"}}
      [:h2 {:style {:color "#7AAAE0" :flex "1" :margin 0}} "JOURNEY — Observe"]
      [:a {:href "/journey"
           :style {:color "#556677" :text-decoration "none" :font-size "13px"}}
       "← home"]]
     (if (seq games)
       [:div {:style {:display "flex" :flex-direction "column" :gap "12px"}}
        (for [g games]
          (let [k       (or (:key g) (str g))
                players (or (:players g) [])
                bots    (or (:bots g) #{})
                cp      (:current-player g)
                round   (:round g)
                flares  (:flares g)]
            [:div {:key k
                   :style {:display "flex" :align-items "center" :gap "10px"
                           :flex-wrap "wrap"}}
             ;; Game info bubble
             [:a {:href (str "/journey/play/" k)
                  :style {:display "inline-block" :padding "8px 16px"
                          :background "#0A0E1C" :border "1px solid #2A4A80"
                          :border-radius "20px" :color "#7AAAE0"
                          :text-decoration "none" :font-size "13px"
                          :font-weight "bold"
                          :transition "background 0.15s"}
                  :on-mouse-over #(set! (.. % -currentTarget -style -background) "#10182A")
                  :on-mouse-out  #(set! (.. % -currentTarget -style -background) "#0A0E1C")}
              k]
             ;; Game stats
             (when (or round flares)
               [:span {:style {:color "#445566" :font-size "10px"}}
                (str (when round (str "r" round))
                     (when (and round flares) " · ")
                     (when flares (str "flares " flares "/13")))])
             ;; Current turn indicator
             (when cp
               [:span {:style {:color "#556677" :font-size "10px"}}
                (str "turn: " cp)])
             ;; Player bubbles
             (for [p players]
               (let [is-bot? (contains? bots p)]
                 [:a {:key p
                      :href (str "/player/" p)
                      :style {:display "inline-block" :padding "5px 12px"
                              :background (if is-bot? "#0A1210" "#0A0E1C")
                              :border (str "1px solid " (if is-bot? "#1A3A2A" "#1A2A40"))
                              :border-radius "16px"
                              :color (if is-bot? "#66AA88" "#AACCEE")
                              :text-decoration "none" :font-size "11px"
                              :transition "background 0.15s"}
                      :on-mouse-over #(set! (.. % -currentTarget -style -background)
                                            (if is-bot? "#102818" "#10182A"))
                      :on-mouse-out  #(set! (.. % -currentTarget -style -background)
                                            (if is-bot? "#0A1210" "#0A0E1C"))}
                  (str p (when is-bot? " ⚙"))]))]))]
       [:div {:style {:text-align "center" :padding "60px 0"}}
        [:p {:style {:color "#445566" :font-size "16px" :margin-bottom "16px"}}
         "No active games to observe."]
        [:div {:style {:display "flex" :gap "12px" :justify-content "center"}}
         [:a {:href "/journey/create"
              :style {:color "#7AAAE0" :padding "8px 20px"
                      :border "1px solid #2A4A80" :border-radius "4px"
                      :text-decoration "none"}}
          "Create a game"]
         [:a {:href "/journey/generate"
              :style {:color "#7AAAE0" :padding "8px 20px"
                      :border "1px solid #2A4A80" :border-radius "4px"
                      :text-decoration "none"}}
          "Generate a game"]]])]))

;; ── WebSocket messages ────────────────────────────────────────────────────────

(defn- append-history! [s]
  (when s
    (let [phase  (game/current-phase s)
          player (game/current-player s)
          step   (count @play-history)]
      (swap! play-history conj
             {:step step :player player
              :phase (or phase :?) :state s}))))

(defn update-messages! [{:keys [type state] :as received}]
  (condp = type
    "game-state"
    (do
      (reset! pending-convert nil)
      ;; Update server-provided choices BEFORE game-state to avoid stale render
      (when (get received :choices)
        (reset! server-choices
                {:keys (reader/read-string (get received :choices))
                 :phase (keyword (subs (get received :phase) 1))}))
      (when (contains? received :bots) (reset! bots-set (set (get received :bots))))
      (let [cu (get received :can-undo)]
        (when (some? cu)
          (js/console.log "can-undo from server:" cu)
          (reset! can-undo? cu)))
      ;; Update game-state last — this triggers the re-render
      (when state
        (let [s (reader/read-string state)]
          (reset! game-state s)
          (append-history! s)
          ;; Trigger landing animation on game-over
          (when (and (= :landing (get-in s [:game-over :type]))
                     (not= @landing-anim-triggered (get-in s [:game-over :tile])))
            (reset! landing-anim-triggered (get-in s [:game-over :tile]))
            (start-landing-animation! s)))))

    "initialize"
    (do
      ;; Update server-provided choices BEFORE game-state
      (when (get received :choices)
        (reset! server-choices
                {:keys (reader/read-string (get received :choices))
                 :phase (keyword (subs (get received :phase) 1))}))
      (when (contains? received :bots) (reset! bots-set (set (get received :bots))))
      (when (contains? received :can-undo) (reset! can-undo? (get received :can-undo)))
      ;; Update game-state last
      (when state
        (let [s (reader/read-string state)]
          (reset! game-state s)
          (let [saved (get received :history [])]
            (reset! play-history (vec saved))
            (append-history! s)))))

    "replay-state"
    (let [step  (get received :step)
          state (get received :state)]
      (when (and step state)
        (let [s (reader/read-string state)]
          ;; Patch the history entry with the replayed state
          (swap! play-history assoc-in [step :state] s))))

    (js/console.log "unknown message" (pr-str received))))

;; ── Generate (simulation replay with choice log) ─────────────────────────────

(defn generate-page []
  (r/with-let
      [history       (when (exists? js/generateHistory)
                       (reader/read-string js/generateHistory))
       n             (count history)
       player-order  (:turn-order (:state (first history)))
       player-colors (board/build-player-colors player-order)
       selected       (r/atom 0)
       playing?       (r/atom false)
       interval       (r/atom nil)
       history-focus? (r/atom false)
       ;; Stop interval without changing playing? flag
       stop-interval!
       (fn []
         (when-let [id @interval]
           (js/clearInterval id)
           (reset! interval nil)))
       ;; Advance one step; auto-stop at end
       advance!
       (fn []
         (let [next (inc @selected)]
           (if (< next n)
             (reset! selected next)
             (do (reset! playing? false)
                 (stop-interval!)))))
       ;; Start playback from current position
       start!
       (fn []
         (stop-interval!)
         (reset! playing? true)
         (reset! interval (js/setInterval advance! 40)))
       ;; Manual step — always pauses playback
       navigate!
       (fn [dir]
         (stop-interval!)
         (reset! playing? false)
         (swap! selected #(case dir
                            :up   (min (dec n) (inc %))
                            :down (max 0 (dec %)))))
       ;; Single capture-phase listener: intercepts keys when history has focus,
       ;; preventing the board's own keydown handler from also firing.
       ;; When history is not focused this handler is a no-op, so the board pans normally.
       on-key
       (fn [e]
         (case (.-key e)
           "Escape"
           (do (.preventDefault e) (swap! history-focus? not))
           (when @history-focus?
             (case (.-key e)
               "ArrowUp"
               (do (.preventDefault e) (.stopImmediatePropagation e) (navigate! :up))
               "ArrowDown"
               (do (.preventDefault e) (.stopImmediatePropagation e) (navigate! :down))
               " "
               (do (.preventDefault e) (.stopImmediatePropagation e)
                   (if @playing?
                     (do (stop-interval!) (reset! playing? false))
                     (start!)))
               nil))))
       _ (js/document.addEventListener "keydown" on-key #js {:capture true})]

      (let [sel     @selected
            focused @history-focus?
            entry   (nth history sel)
            state   (:state entry)]
        ;; Outer mousedown clears history focus; panel mousedown stops propagation
        [:div {:style    {:display "flex" :width "100vw" :height "100vh"
                          :background "#04040E" :overflow "hidden"}
               :on-mouse-down #(reset! history-focus? false)}

         ;; Board — no on-navigate; Up/Down always pan when history is not focused
         [:div {:style {:flex "1" :overflow "hidden"}}
          [board-view state #{} nil nil
           {:cipher-on-beacon-hover (fn [pos color]
                                      (reset! cipher-hover
                                              (when pos {:pos pos :color color})))
            :cipher-hover @cipher-hover}]]

         ;; History panel — mousedown stops propagation so outer blur doesn't fire
         [:div {:style         {:width "250px" :display "flex" :flex-direction "column"
                                :background "#05060F" :border-left "1px solid #141830"
                                :outline (if focused "1px solid #1E3A5A" "none")}
                :on-mouse-down #(.stopPropagation %)}

          ;; Header: step counter + play/pause
          [:div {:style {:display "flex" :align-items "center"
                         :padding "6px 10px 5px"
                         :border-bottom "1px solid #141830"}}
           [:span {:style {:flex "1" :color "#334455" :font-size "9px"
                           :font-family "monospace" :letter-spacing "1.5px"
                           :text-transform "uppercase"}}
            (str "step " (:step entry) " / " (dec n))]
           [:button
            {:on-click #(if @playing? (do (stop-interval!) (reset! playing? false)) (start!))
             :style    {:background "none" :border "1px solid #1E2A3A"
                        :border-radius "3px" :color "#445566"
                        :font-size "12px" :cursor "pointer"
                        :padding "1px 7px" :font-family "monospace"
                        :line-height "1.4"}}
            (if @playing? "⏸" "▶")]]

          ;; Scrollable step list — newest entry on top, oldest at bottom
          ;; Keep selected item in view after every render
          (do
            (r/after-render
             #(when-let [el (.getElementById js/document (str "hist-item-" sel))]
                (.scrollIntoView el #js {:block "nearest" :behavior "instant"})))
            [:div {:style {:flex "1" :overflow-y "auto" :padding "4px 0"}}
             (for [i (range (dec n) -1 -1)]
               (let [{:keys [player] :as entry} (nth history i)
                     above-player (:player (when (< i (dec n)) (nth history (inc i))))
                     group-start? (not= player above-player)
                     show-sep?    (and group-start? (< i (dec n)))]
                 ^{:key i}
                 [history-row i entry player-colors (= i sel) group-start? show-sep?
                  #(do (stop-interval!)
                       (reset! playing? false)
                       (reset! history-focus? true)
                       (reset! selected i))]))])]])

      (finally
        (stop-interval!)
        (js/document.removeEventListener "keydown" on-key #js {:capture true}))))

;; ── Page container ────────────────────────────────────────────────────────────

(defn page-container []
  (cond
    (and (exists? js/isGenerate) js/isGenerate) [generate-page]
    (and (exists? js/isObserve)  js/isObserve)  [observe-page]
    (and (exists? js/isCreate)   js/isCreate)   [create-page]
    (and (exists? js/playKey)    js/playKey)     [play-page]
    :else [:div]))

;; ── Init ──────────────────────────────────────────────────────────────────────

(defn mount-components []
  (rdom/render [#'page-container] (.getElementById js/document "journey")))

(defn init! []
  (ajax/load-interceptors!)
  (when js/playerPreferences
    (reset! player-preferences (reader/read-string js/playerPreferences)))
  (when js/isObserve
    (when js/observeGames
      (reset! observe-games (reader/read-string js/observeGames))))
  (when js/playKey
    (let [protocol (if (= (.-protocol js/location) "https:") "wss:" "ws:")]
      (ws/make-websocket!
       (str protocol "//" (.-host js/location) "/ws/journey/play/" js/playKey)
       update-messages!)))
  (mount-components))
