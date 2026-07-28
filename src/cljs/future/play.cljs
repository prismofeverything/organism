(ns future.play
  "Reagent frontend for FUTURE. Four pages, dispatched by JS globals set
   in the HTML shell:

     js/playKey     — /future/play/:play   live game (WebSocket)
     js/isCreate    — /future/create       new-game lobby
     js/isObserve   — /future/observe      observer list
     js/isGenerate  — /future/generate     local bot simulator

   All player choices are made by clicking things on the board. The side
   panel holds status + reference info only — no action buttons."
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [organism.websockets :as ws]
   [organism.ajax :as ajax]
   [organism.components :as components]
   [future.board :as board]
   [future.game :as game]))

;; ── Atoms ─────────────────────────────────────────────────────────────────

(defonce game-state         (r/atom nil))
(defonce player-key         (r/atom nil))
(defonce legal-actions-atom (r/atom {}))
(defonce connection-status  (r/atom :disconnected))
(defonce error-log          (r/atom []))
(defonce action-log         (r/atom []))

;; Board topology is static — computed once, spliced into every state
;; received from the server (which strips it to save ~72% wire size).
(defonce cached-board (delay (board/build-board)))

(defn- with-board [st]
  (cond-> st
    (and st (not (:board st))) (assoc :board @cached-board)))

;; SVG path strings for each orbital space are pure functions of sid.
;; Compute once at load time; look up per render instead of running
;; `arc_path` (trig) 86 times per frame.
(def ^:private orbit-path-cache
  (into {}
    (for [ring board/orbits
          i (range (board/ring-sizes ring))
          :let [sid (board/orbit-space ring i)]]
      [sid (board/orbit-space-path sid)])))

(defn- cached-orbit-path [sid] (orbit-path-cache sid))

;; ── Perf profiling ────────────────────────────────────────────────────────
;; Toggle with (swap! profile? not) from the JS console.

;; Set to true from the JS console to log per-render timings:
;;   future.play.profile_QMARK_.state = true
(defonce profile? (r/atom false))

(defn- now-ms [] (js/performance.now))

(defn- log-phase!
  "Print a timing bucket to the console when profiling is on."
  [label t0]
  (when @profile?
    (js/console.log (str label ": " (.toFixed (- (now-ms) t0) 2) "ms"))))

(defonce render-counter (atom 0))

;; Staged source for two-click flows (fly src→dst, path, link src→dst,
;; plus the exhaust-pk pick for link-placement).
;;   {:kind :sundiver :sid sid :idx idx :sundiver sd}
;;   {:kind :link-src :sid sid :awaiting-dst dst-sid}
(defonce pending            (r/atom nil))

;; Server-reported undo depth (WS mode). Local mode uses (count @local-history).
(defonce undo-depth-atom    (r/atom 0))
(defonce local-history      (r/atom []))
(defonce mode-atom          (r/atom :ws))   ; :ws or :local

;; ── Player identity colors ────────────────────────────────────────────────

(def player-fill
  {:silver "#dddddd"
   :green  "#4fd85f"
   :blue   "#4f8fef"
   :purple "#af4fef"
   :void   "#888888"})

(defn player-color [state pk]
  (player-fill (get-in state [:players pk :wedge-color])))

;; ── Helpers ───────────────────────────────────────────────────────────────

(defn- safe-read [s]
  (try (reader/read-string s)
       (catch :default e
         (swap! error-log conj (str "parse error: " e))
         nil)))

(defn- safe-name [k]
  (cond (keyword? k) (name k)
        (string? k)  k
        :else        (str k)))

(defn- sid->str [sid]
  (cond
    (and (vector? sid) (= :orbit (first sid)))
    (str (safe-name (second sid)) "·" (nth sid 2))

    (and (vector? sid) (= :sun (first sid)))
    (str "sun" (second sid))

    :else (pr-str sid)))

(defn- action-label
  "Human-readable label for a choice-key (used in the log)."
  [k]
  (cond
    (not (vector? k)) (pr-str k)
    :else
    (let [verb (first k)
          args (rest k)]
      (case verb
        :place-mothership     (str "place @ " (sid->str (first args)))
        :stay                 "stay"
        :shift-in             "shift in"
        :shift-out            "shift out"
        :choose-move          "MOVE"
        :choose-activate      "ACTIVATE"
        :launch               (str "launch → " (sid->str (first args)))
        :fly                  (str "fly " (sid->str (first args)) " → " (sid->str (second args)))
        :path                 (str "path " (sid->str (first args)) " → " (sid->str (second args)))
        :planet-on            (str "board @ " (sid->str (first (first args))))
        :planet-off           (str "off @ " (sid->str (first (first args))))
        :done-moving          "done moving"
        :activate-sun         "sun"
        :activate-planets     "planets"
        :activate-cities      "cities"
        :activate-space       (str "activate " (sid->str (first args)))
        :done-activating      "done activating"
        :no-activation-possible "no activation possible"
        :sun-outer            (str "sun-outer #" (first args))
        :sun-inner            (str "sun-inner #" (first args))
        :planet-buy           (str "buy #" (first args))
        :planet-build         (str "build " (safe-name (second args)) " #" (first args))
        :link                 (str "link "
                                   (sid->str (first args))
                                   " → "
                                   (sid->str (second args))
                                   (when (nth args 2 nil)
                                     (str " (exhaust " (safe-name (nth args 2)) ")")))
        :done-linking         "done linking"
        :take-bonus           "take bonus"
        :decline-bonus        "decline bonus"
        :draw-next            "draw"
        :orbit-resolved       "orbit"
        :advance-resolved     "advance"
        :begin-next-turn      "next turn"
        :undo                 "↶ undo"
        :end                  "END"
        (pr-str k)))))

;; ── WebSocket ─────────────────────────────────────────────────────────────

(defn- push-log! [action-key state]
  (when state
    (swap! action-log conj
           {:turn (:turn state)
            :player (:flame state)
            :phase (:phase state)
            :action (action-label action-key)})))

(defn receive-message! [msg]
  (let [type (get msg "type")]
    (case type
      "initialize"
      (do (reset! connection-status :connected)
          (reset! player-key (get msg "player"))
          (when-let [ud (get msg "undo-depth")]
            (reset! undo-depth-atom (js/parseInt ud)))
          (when-let [s (get msg "state")]
            (let [st (with-board (safe-read s))]
              (reset! game-state st)
              (reset! legal-actions-atom (game/legal-actions st))
              (reset! pending nil))))

      "game-state"
      (when-let [s (get msg "state")]
        (let [t0 (now-ms)
              raw-len (count s)
              parsed (safe-read s)
              t1 (now-ms)
              st (with-board parsed)
              ak (when-let [a (get msg "action")] (safe-read a))
              old-cur (game/current-player @game-state)
              new-cur (game/current-player st)]
          (when ak (push-log! ak @game-state))
          (when-let [ud (get msg "undo-depth")]
            (reset! undo-depth-atom (js/parseInt ud)))
          (reset! game-state st)
          (let [t2 (now-ms)]
            (reset! legal-actions-atom (game/legal-actions st))
            (let [t3 (now-ms)]
              (when @profile?
                (js/console.log
                  (str "RX[" raw-len "B] parse=" (.toFixed (- t1 t0) 1)
                       " la=" (.toFixed (- t3 t2) 1)
                       " total=" (.toFixed (- t3 t0) 1) "ms"
                       " phase=" (name (:phase st))
                       " actions=" (count @legal-actions-atom))))))
          (when (not= old-cur new-cur)
            (reset! pending nil))))

      (println "future: unknown message type" type))))

(defn connect-ws! [pk]
  (let [proto (if (= "https:" (.-protocol js/location)) "wss:" "ws:")
        host  (.-host js/location)
        url   (str proto "//" host "/ws/future/play/" pk)]
    (ws/make-websocket! url receive-message!)))

;; ── Local (generate) mode helpers ────────────────────────────────────────

(defn- push-local-history! [state]
  (when state (swap! local-history conj state)))

;; ── Dispatch: WS or local ────────────────────────────────────────────────

(defn dispatch-chain!
  "Apply a chain of choice-keys. In WS mode, sends to server. In local
   mode, walks legal-actions per step and pushes history."
  [chain]
  (when (seq chain)
    (case @mode-atom
      :ws
      (ws/send-transit-message!
        (if (= 1 (count chain))
          {"type" "action" "choice" (pr-str (first chain))}
          {"type" "action" "chain" (pr-str (vec chain))}))

      :local
      (loop [s @game-state
             remaining chain
             last-ck nil]
        (if (empty? remaining)
          (do
            (reset! game-state s)
            (reset! legal-actions-atom (game/legal-actions s))
            (reset! pending nil)
            (when last-ck (push-log! last-ck s)))
          (let [ck (first remaining)
                actions (game/legal-actions s)
                nxt (game/next-state actions ck)]
            (if nxt
              (do
                (push-local-history! s)
                (recur nxt (rest remaining) ck))
              (do
                (swap! error-log conj (str "illegal action: " (pr-str ck)
                                           " from phase " (pr-str (:phase s))))
                (reset! pending nil)))))))))

(defn dispatch-choice! [ck] (dispatch-chain! [ck]))

(defn dispatch-undo! []
  (case @mode-atom
    :ws
    (ws/send-transit-message! {"type" "undo"})

    :local
    (when (seq @local-history)
      (let [prev (peek @local-history)]
        (swap! local-history pop)
        (reset! game-state prev)
        (reset! legal-actions-atom (game/legal-actions prev))
        (reset! pending nil)
        (push-log! [:undo] prev)))))

(defn undo-depth []
  (case @mode-atom
    :ws @undo-depth-atom
    :local (count @local-history)))

;; ── Legal-action queries ─────────────────────────────────────────────────

(defn- action-matches
  "Find the first choice-key in actions where (pred ck) is truthy."
  [actions pred]
  (some (fn [[k _]] (when (pred k) k)) actions))

(defn- has-action? [actions ck] (contains? actions ck))

(defn- has-verb? [actions verb]
  (action-matches actions (fn [k] (and (vector? k) (= verb (first k))))))

;; ── Choice derivation from clicks ────────────────────────────────────────
;;
;; Each derivation returns a chain (vector of choice-keys) or nil. The
;; chain includes intermediate phase-branch choices (:choose-activate,
;; :activate-sun/planets/cities) so the whole compound action lands as
;; one atomic server call.

(defn- launch-target-set
  "Set of sids valid as :launch destinations right now."
  [state]
  (let [player (game/current-player state)]
    (set (game/launch-targets state player))))

(defn- move-space-chain
  "Chain for clicking `sid` — works both from :moving directly and from
   :choose-action-type (in which case we prepend :choose-move so the
   whole thing lands atomically)."
  [state actions sid]
  (let [pnd @pending
        need-move? (or (= :choose-action-type (:phase state))
                       (and pnd (:need-move-branch? pnd)))
        moving-actions (if need-move?
                         (some-> (game/next-state actions [:choose-move])
                                 game/legal-actions)
                         actions)
        launch-targets (launch-target-set state)
        prefix (if need-move? [[:choose-move]] [])]
    (cond
      ;; Two-click completion: pending is a sundiver, clicked sid is a dst
      (and pnd (= :sundiver (:kind pnd)))
      (let [src (:sid pnd)
            fly [:fly src sid]
            path [:path src sid]]
        (cond
          (has-action? moving-actions fly)  (conj (vec prefix) fly)
          (has-action? moving-actions path) (conj (vec prefix) path)
          :else nil))

      ;; No selection: click a launch target
      (and (nil? pnd) (contains? launch-targets sid)
           (has-action? moving-actions [:launch sid]))
      (conj (vec prefix) [:launch sid]))))

(defn- activate-target-for-sid
  "For a click on a sundiver at sid, in the untargeted `:activating`
   phase, which sub-target keyword applies?"
  [state sid]
  (cond
    (board/sun? sid)                           :sun
    (some (fn [[_ p]] (= p sid)) (:planets state)) :planets
    (game/city-at state sid)                   :cities))

(defn- ensure-activate-target
  "If :activating with target unset, return the target-picker chain
   [:choose-activate + :activate-*]; else []. Assumes current phase
   is one of the compound activation flows."
  [state kind]
  (case (:phase state)
    :choose-action-type
    (cond-> [[:choose-activate]]
      kind (conj ({:sun     [:activate-sun]
                   :planets [:activate-planets]
                   :cities  [:activate-cities]} kind)))
    :activating
    (if (nil? (get-in state [:phase-data :target]))
      [({:sun     [:activate-sun]
         :planets [:activate-planets]
         :cities  [:activate-cities]} kind)]
      [])
    []))

(defn- activate-space-chain
  "Chain for activating `sid` — includes the target-picker prefix if
   we're still in :choose-action-type or an untargeted :activating."
  [state sid]
  (let [kind (activate-target-for-sid state sid)]
    (when kind
      (let [prefix (ensure-activate-target state kind)]
        (conj (vec prefix) [:activate-space sid])))))

(defn- space-click-chain
  "Given the current state + click on `sid`, return the chain of
   choice-keys to send, or nil for 'nothing legal here'. Handles the
   phase-implicit MOVE vs ACTIVATE branch by peeking at what `sid`
   would enable."
  [state actions sid]
  (case (:phase state)
    :place-mothership
    (when (has-action? actions [:place-mothership sid])
      [[:place-mothership sid]])

    :resolve-mothership
    (let [player (game/current-player state)
          ms     (game/mothership-of state player)
          adj    (get-in state [:board :adjacency])
          r      (board/orbit-of ms)
          inner-t (when (and r (not= :silver r) (board/inner-orbit r))
                    (board/frontmost-adjacent-in-ring adj ms (board/inner-orbit r)))
          outer-t (when (and r (board/outer-orbit r))
                    (board/frontmost-adjacent-in-ring adj ms (board/outer-orbit r)))]
      (cond
        (and (= sid ms) (has-action? actions [:stay]))           [[:stay]]
        (and (= sid inner-t) (has-action? actions [:shift-in]))  [[:shift-in]]
        (and (= sid outer-t) (has-action? actions [:shift-out])) [[:shift-out]]))

    :choose-action-type
    (let [pnd @pending
          pk (game/current-player state)
          mine? (game/player-has-sundiver-at? state sid pk)
          activate-kind (when mine? (activate-target-for-sid state sid))
          launch-ok? (contains? (launch-target-set state) sid)]
      (cond
        ;; A sundiver is already staged → this is a destination click
        (and pnd (= :sundiver (:kind pnd)))
        (move-space-chain state actions sid)

        activate-kind
        (concat [[:choose-activate]
                 ({:sun     [:activate-sun]
                   :planets [:activate-planets]
                   :cities  [:activate-cities]} activate-kind)]
                [[:activate-space sid]])

        launch-ok?
        [[:choose-move] [:launch sid]]))

    :moving
    (move-space-chain state actions sid)

    :activating
    (let [pk (game/current-player state)]
      (when (game/player-has-sundiver-at? state sid pk)
        (activate-space-chain state sid)))

    ;; sun/planet sub-phases: click a space to switch which one you're
    ;; activating is not legal — that happens by target sub-picker.
    nil))

(defn- flame-click-chain
  "Clicking the flame is 'commit / advance'. Maps to whichever done-*
   action is available in the current phase."
  [state actions]
  (cond
    (and (= :resolve-mothership (:phase state))
         (has-action? actions [:stay]))
    [[:stay]]

    (and (= :moving (:phase state)) (has-action? actions [:done-moving]))
    [[:done-moving]]

    (and (= :activating (:phase state)) (has-action? actions [:done-activating]))
    [[:done-activating]]

    (and (= :link-placement (:phase state)) (has-action? actions [:done-linking]))
    [[:done-linking]]

    (and (contains? #{:owner-bonus-decision :activator-bonus-decision} (:phase state))
         (has-action? actions [:decline-bonus]))
    [[:decline-bonus]]

    (and (= :game-over (:phase state))
         (has-action? actions [:end]))
    [[:end]]))

(defn- sundiver-click-chain
  "Chain for clicking a sundiver at sid, idx idx."
  [state actions sid idx sundiver]
  (let [pk (game/current-player state)
        mine? (= pk (:owner sundiver))]
    (case (:phase state)
      :moving
      (cond
        ;; on-planet toggle: sundiver on planet-space, choice available
        (and mine? (has-action? actions [:planet-on [sid idx]]))
        [[:planet-on [sid idx]]]

        (and mine? (has-action? actions [:planet-off [sid idx]]))
        [[:planet-off [sid idx]]]

        ;; select as source for fly/path
        mine? (do (reset! pending {:kind :sundiver :sid sid :idx idx :sundiver sundiver})
                  nil))

      :choose-action-type
      ;; Two things a sundiver-click could mean: activate (if on target
      ;; type) or start MOVE by staging as fly source. Prefer activate.
      (cond
        (and mine? (activate-target-for-sid state sid))
        (activate-space-chain state sid)

        mine?
        (do (reset! pending {:kind :sundiver :sid sid :idx idx :sundiver sundiver})
            ;; If we're going to fly this sundiver we need :choose-move
            ;; queued for when they click the dst. Stash a hint.
            (swap! pending assoc :need-move-branch? true)
            nil))

      :activating
      (when (and mine? (activate-target-for-sid state sid))
        (activate-space-chain state sid))

      :activating-sun-space
      ;; Click the sundiver → default to outer (also click inner-half
      ;; separately to trigger sun-inner). If the sundiver has a
      ;; matching resource + can do inner, still default outer here.
      (cond
        (has-action? actions [:sun-outer idx]) [[:sun-outer idx]]
        (has-action? actions [:sun-inner idx]) [[:sun-inner idx]])

      :activating-planet-space
      (cond
        (has-action? actions [:planet-buy idx])
        [[:planet-buy idx]]

        (:resource sundiver)
        (let [rc (:resource sundiver)
              ck [:planet-build idx rc]]
          (when (has-action? actions ck) [ck])))

      :link-placement
      (when mine?
        (do (reset! pending {:kind :sundiver :sid sid :idx idx :sundiver sundiver})
            nil))

      nil)))

(defn- wedge-half-click-chain
  "sid must be a :sun wedge. `half` is :inner or :outer."
  [state actions sid half]
  (case (:phase state)
    :activating-sun-space
    (let [current (get-in state [:phase-data :current])
          divs (game/sundivers-at state sid)
          pk   (game/current-player state)
          my-divs (map-indexed vector divs)
          my-idxs (filter (fn [[_ d]] (= pk (:owner d))) my-divs)
          matching (filter (fn [[_ d]]
                             (= (:resource d) (board/wedge-color (board/wedge-of sid))))
                           my-idxs)
          any-idx (some (fn [[i _]] (when (has-action? actions [:sun-outer i]) i)) my-idxs)
          matching-idx (some (fn [[i _]] (when (has-action? actions [:sun-inner i]) i)) matching)]
      (when (= sid current)
        (case half
          :inner (when matching-idx [[:sun-inner matching-idx]])
          :outer (when any-idx [[:sun-outer any-idx]]))))
    ;; Otherwise fall through to normal space click on the wedge.
    (space-click-chain state actions sid)))

(defn- link-src-legal-here?
  "Would clicking `sid` as a link source enable any :link action?"
  [actions sid]
  (some (fn [[k _]]
          (and (vector? k) (= :link (first k)) (= sid (nth k 1))))
        actions))

(defn- link-src-click-chain
  "In link-placement phase, clicking a space that is a legal link source
   stages it. Returns nil (staged, not sent)."
  [_state actions sid]
  (when (link-src-legal-here? actions sid)
    (reset! pending {:kind :link-src :sid sid})
    nil))

(defn- link-dst-click-chain
  "In link-placement, second click: complete link. Handles the exhaust-pk
   variant by picking the actor first if legal, else picking any owner
   with an active matching component (auto)."
  [actions src dst]
  (let [plain [:link src dst]
        with-exh (some (fn [[k _]]
                         (and (vector? k) (= :link (first k))
                              (= 4 (count k))
                              (= src (nth k 1)) (= dst (nth k 2))))
                       actions)]
    (cond
      (has-action? actions plain) [plain]
      with-exh                    [with-exh])))

;; ── SVG board ─────────────────────────────────────────────────────────────

(def view-size board/view-size)
(def vcenter board/center)

(defn- background []
  [:rect {:width view-size :height view-size :fill "#06070d"}])

(defn- beam-line []
  [:line {:x1 vcenter :y1 (- vcenter board/sun-inner-r)
          :x2 vcenter :y2 (- vcenter (second (board/orbit-radii :void)))
          :stroke board/beam-color :stroke-width 2 :opacity 0.55}])

(defn- ring-divider [r]
  [:circle {:cx vcenter :cy vcenter :r r
            :fill "none" :stroke "#000" :stroke-width 1 :opacity 0.4}])

(defn- shift-target-in [state pk ms]
  (when ms
    (let [r (board/orbit-of ms)
          inner (board/inner-orbit r)]
      (when (and inner (not= :silver r))
        (board/frontmost-adjacent-in-ring
          (get-in state [:board :adjacency]) ms inner)))))

(defn- shift-target-out [state pk ms]
  (when ms
    (let [r (board/orbit-of ms)
          outer (board/outer-orbit r)]
      (when outer
        (board/frontmost-adjacent-in-ring
          (get-in state [:board :adjacency]) ms outer)))))

(defn- derive-clickable
  "Compute derived render sets ONCE per (state, actions, pending) —
   avoids per-space recomputation in the render loop.

   Returns:
     {:clickable-sids  #{sid ...}    ; spaces that light up
      :dst-sids        #{sid ...}    ; subset that are second-click dsts
      :link-src-sids   #{sid ...}    ; legal :link starts (for overlays)
      :ms-by-sid       {sid → pk}    ; who has a mothership where
      :city-by-sid     :cities of state (alias)
      :planet-sids     #{sid ...}    ; where each planet currently is
      :shift-in-sid    sid|nil
      :shift-out-sid   sid|nil
      :launch-sids     #{sid ...}}"
  [state actions pnd]
  (let [phase (:phase state)
        pk    (game/current-player state)
        ms    (game/mothership-of state pk)
        adj   (get-in state [:board :adjacency])
        link-src-sids
        (into #{}
              (keep (fn [[k _]]
                      (when (and (vector? k) (= :link (first k)))
                        (nth k 1))))
              actions)
        launch-sids (set (game/launch-targets state pk))
        shift-in-sid (shift-target-in state pk ms)
        shift-out-sid (shift-target-out state pk ms)
        planet-sids (into #{} (vals (:planets state)))
        ms-by-sid (reduce-kv (fn [m p pd]
                               (let [msid (:mothership pd)]
                                 (if (and msid (not= msid :supply))
                                   (assoc m msid p) m)))
                             {} (:players state))
        clickable
        (cond
          ;; Staged link src → dsts are the [:link src dst ...] targets
          (and (= phase :link-placement) pnd (= :link-src (:kind pnd)))
          (into #{}
                (keep (fn [[k _]]
                        (when (and (vector? k) (= :link (first k))
                                   (= (:sid pnd) (nth k 1)))
                          (nth k 2))))
                actions)

          ;; Staged sundiver → dsts are adjacent + path targets from src
          (and (or (= phase :moving) (= phase :choose-action-type))
               pnd (= :sundiver (:kind pnd)))
          (let [src (:sid pnd)
                adj-set (get adj src #{})
                path-set (into #{}
                               (keep (fn [[k _]]
                                       (when (and (vector? k)
                                                  (= :path (first k))
                                                  (= src (nth k 1)))
                                         (nth k 2))))
                               actions)]
            (into adj-set path-set))

          :else
          (into #{}
                (concat
                  ;; place-mothership targets
                  (keep (fn [[k _]]
                          (when (and (vector? k) (= :place-mothership (first k)))
                            (nth k 1)))
                        actions)
                  ;; explicit launch targets in :moving
                  (keep (fn [[k _]]
                          (when (and (vector? k) (= :launch (first k)))
                            (nth k 1)))
                        actions)
                  ;; choose-action-type: launches + activate-eligible sundiver spaces
                  (when (= phase :choose-action-type) launch-sids)
                  (when (and pk
                             (contains? #{:choose-action-type :activating} phase))
                    (for [sid (keys (:sundivers state))
                          :when (and (game/player-has-sundiver-at? state sid pk)
                                     (or (board/sun? sid)
                                         (contains? planet-sids sid)
                                         (game/city-at state sid)))]
                      sid))
                  ;; resolve-mothership: current + shift targets
                  (when (= phase :resolve-mothership)
                    (keep identity [ms shift-in-sid shift-out-sid]))
                  ;; link-placement (no pending): sources
                  (when (and (= phase :link-placement) (nil? pnd))
                    link-src-sids))))
        dst-sids (when (and pnd
                            (or (and (= :link-src (:kind pnd)) (= :link-placement phase))
                                (and (= :sundiver (:kind pnd))
                                     (contains? #{:moving :choose-action-type} phase))))
                   clickable)]
    {:clickable-sids clickable
     :dst-sids       (or dst-sids #{})
     :link-src-sids  link-src-sids
     :ms-by-sid      ms-by-sid
     :city-by-sid    (:cities state)
     :planet-sids    planet-sids
     :shift-in-sid   shift-in-sid
     :shift-out-sid  shift-out-sid
     :launch-sids    launch-sids}))

(defn- orbital-space-component [sid state actions derived]
  (let [d       (cached-orbit-path sid)
        city    (get (:city-by-sid derived) sid)
        ms-owner (get (:ms-by-sid derived) sid)
        planet?  (contains? (:planet-sids derived) sid)
        base    (get board/orbit-colors (board/orbit-of sid))
        highlight? (contains? (:clickable-sids derived) sid)
        dst?       (contains? (:dst-sids derived) sid)
        fill    (cond
                  ms-owner (player-color state ms-owner)
                  city     (get board/orbit-colors (:color city))
                  :else    base)
        stroke  (cond
                  dst?       "#ffe066"
                  highlight? "#8fddff"
                  planet?    "#ffffff"
                  :else      "#000")
        sw      (cond
                  dst?       2.4
                  highlight? 2.0
                  planet?    1.6
                  :else      0.5)]
    [:path {:d d
            :fill fill
            :fill-opacity (if city 0.9 0.62)
            :stroke stroke
            :stroke-width sw
            :style {:cursor (if highlight? "pointer" "default")}
            :on-click (fn [e]
                        (.stopPropagation e)
                        (let [chain (space-click-chain state actions sid)]
                          (when (seq chain) (dispatch-chain! chain))))}]))

(defn- wedge-half-highlight
  "Highlight for a specific half of a sun wedge."
  [state actions derived sid half]
  (cond
    (and (= :activating-sun-space (:phase state))
         (= sid (get-in state [:phase-data :current])))
    (boolean (wedge-half-click-chain state actions sid half))

    :else (contains? (:clickable-sids derived) sid)))

(defn- wedge-component-tokens
  "Small player-colored dots for components in one half of a wedge.
   `half-side` = :active or :exhausted controls where the row is placed."
  [state sid k half-side counts]
  (let [entries (for [[pk n] counts
                      :when (pos? n)
                      i (range n)]
                  [pk i])
        n (count entries)
        [cx cy] (board/space-center sid)
        ;; Active dots go closer to sun center (inner triangle), exhausted
        ;; dots go further out (outer red ring).
        row-r (case half-side
                :active   (+ (/ board/sun-inner-r 2.0))
                :exhausted (+ board/sun-inner-r
                              (/ (- board/sun-outer-r board/sun-inner-r) 2.0)))
        ;; angular center of this wedge, offset by (k - 0.5)/5
        theta-c (* 2.0 Math/PI (mod (/ (- k 0.5) 5.0) 1.0))
        ;; convert polar → screen (angle 0 = up)
        wedge-cx (+ board/center (* row-r (Math/sin theta-c)))
        wedge-cy (- board/center (* row-r (Math/cos theta-c)))
        span 12
        step (if (<= n 1) 0 (/ span (dec n)))]
    (into [:g {:style {:pointer-events "none"}}]
      (for [[i [pk _]] (map-indexed vector entries)]
        (let [dx (- (* i step) (/ span 2.0))
              tx (+ wedge-cx (* dx (Math/cos (+ theta-c (/ Math/PI 2)))))
              ty (+ wedge-cy (* dx (Math/sin (+ theta-c (/ Math/PI 2)))))]
          [:circle {:key (str "cmp-" (name half-side) "-" k "-" i)
                    :cx tx :cy ty :r 3.2
                    :fill (player-color state pk)
                    :stroke "#000" :stroke-width 0.6}])))))

(defn- wedge-component [sid state actions derived]
  (let [k       (board/wedge-of sid)
        col     (get board/orbit-colors (board/wedge-color k))
        inner-d (board/wedge-triangle-path k)
        outer-d (board/wedge-outer-path k)
        sn      (get-in state [:solar-network k] {:active {} :exhausted {}})
        outer-hl (wedge-half-highlight state actions derived sid :outer)
        inner-hl (wedge-half-highlight state actions derived sid :inner)]
    [:g {:key (str "wedge-" k)}
     [:path {:d outer-d
             :fill board/sun-outer-color :fill-opacity 0.95
             :stroke (if outer-hl "#8fddff" "#000")
             :stroke-width (if outer-hl 2.0 0.8)
             :on-click (fn [e]
                         (.stopPropagation e)
                         (let [ch (wedge-half-click-chain state actions sid :outer)]
                           (when (seq ch) (dispatch-chain! ch))))
             :style {:cursor (if outer-hl "pointer" "default")}}]
     [:path {:d inner-d :fill col :fill-opacity 0.95
             :stroke (if inner-hl "#8fddff" "#000")
             :stroke-width (if inner-hl 2.0 0.8)
             :on-click (fn [e]
                         (.stopPropagation e)
                         (let [ch (wedge-half-click-chain state actions sid :inner)]
                           (when (seq ch) (dispatch-chain! ch))))
             :style {:cursor (if inner-hl "pointer" "default")}}]
     [wedge-component-tokens state sid k :active    (:active sn)]
     [wedge-component-tokens state sid k :exhausted (:exhausted sn)]]))

(defn- planet-toggle-chain
  "If the current player has a sundiver at this planet-space that can
   flip on/off, return the toggle chain."
  [state actions sid]
  (let [divs (game/sundivers-at state sid)
        pk (game/current-player state)
        on-idx  (some (fn [[i sd]]
                        (when (and (= pk (:owner sd))
                                   (has-action? actions [:planet-off [sid i]]))
                          i))
                      (map-indexed vector divs))
        off-idx (some (fn [[i sd]]
                        (when (and (= pk (:owner sd))
                                   (has-action? actions [:planet-on [sid i]]))
                          i))
                      (map-indexed vector divs))]
    (cond
      on-idx  [[:planet-off [sid on-idx]]]
      off-idx [[:planet-on [sid off-idx]]])))

(defn- planet-marker
  "Clickable planet — behavior:
    - if a sundiver is staged and this space is a valid move dst → move here
    - else if we can activate this space → activate
    - else if we can toggle a sundiver on/off here → toggle
    - else nothing"
  [state actions orbit sid]
  (let [[x y] (board/space-center sid)
        move-chain   (space-click-chain state actions sid)
        toggle-chain (planet-toggle-chain state actions sid)
        chain (or move-chain toggle-chain)
        clickable? (boolean chain)]
    [:circle {:cx x :cy y :r 16
              :fill (get board/planet-fill orbit)
              :stroke (if clickable? "#ffe066" "#ffffff")
              :stroke-width (if clickable? 3 2.5)
              :style {:cursor (if clickable? "pointer" "default")}
              :on-click (fn [e]
                          (.stopPropagation e)
                          (when (seq chain) (dispatch-chain! chain)))}]))

(defn- city-marker [sid c]
  (let [[x y] (board/space-center sid)]
    [:g {:style {:pointer-events "none"}}
     [:rect {:x (- x 9) :y (- y 9) :width 18 :height 18
             :fill (get board/orbit-colors (:color c))
             :stroke "#000" :stroke-width 1.4}]
     [:text {:x x :y (+ y 5) :text-anchor "middle"
             :font-size 12 :font-family "monospace" :fill "#000"} "C"]]))

(defn- mothership-marker
  "Directional mothership — half-oval / arrow pointing toward the front
   space (CCW = forward). Non-interactive; passes clicks through."
  [state pk sid]
  (let [[cx cy] (board/space-center sid)
        [fx fy] (board/space-center (board/front-space sid))
        dx (- fx cx) dy (- fy cy)
        len (Math/sqrt (+ (* dx dx) (* dy dy)))
        ux (if (zero? len) 0 (/ dx len))
        uy (if (zero? len) -1 (/ dy len))
        ;; perpendicular (rotated 90° CW): (uy, -ux)
        px uy py (- ux)
        r 16
        ;; nose is a bit past center in forward direction
        nose-x (+ cx (* r 1.1 ux))
        nose-y (+ cy (* r 1.1 uy))
        ;; base = flat line perpendicular to direction, r behind center
        base-cx (- cx (* r 0.4 ux))
        base-cy (- cy (* r 0.4 uy))
        base-l-x (+ base-cx (* r 0.85 px))
        base-l-y (+ base-cy (* r 0.85 py))
        base-r-x (- base-cx (* r 0.85 px))
        base-r-y (- base-cy (* r 0.85 py))
        ;; shoulder points (near the nose, but not at it) to make it a
        ;; blunt arrowhead not a sharp triangle
        sh-l-x (+ cx (* r 0.4 ux) (* r 0.7 px))
        sh-l-y (+ cy (* r 0.4 uy) (* r 0.7 py))
        sh-r-x (- (+ cx (* r 0.4 ux)) (* r 0.7 px))
        sh-r-y (- (+ cy (* r 0.4 uy)) (* r 0.7 py))
        pc    (player-color state pk)]
    [:g {:style {:pointer-events "none"}}
     [:polygon {:points (str nose-x "," nose-y " "
                             sh-r-x "," sh-r-y " "
                             base-r-x "," base-r-y " "
                             base-l-x "," base-l-y " "
                             sh-l-x "," sh-l-y)
                :fill pc
                :stroke "#fff"
                :stroke-width 2}]]))

(defn- triangle-points
  "Points for a triangle at (cx,cy) with radius r, pointing up."
  [cx cy r]
  (let [top-y (- cy r)
        bl-x (- cx (* r 0.866))
        br-x (+ cx (* r 0.866))
        b-y  (+ cy (* r 0.5))]
    (str top-y ; sentinel: won't parse — replace below
         )))

(defn- tri-points [cx cy r]
  (str cx "," (- cy r) " "
       (+ cx (* r 0.866)) "," (+ cy (* r 0.5)) " "
       (- cx (* r 0.866)) "," (+ cy (* r 0.5))))

(defn- sundiver-clickable?
  "Pure predicate — does clicking this sundiver do anything right now?
   No side effects. Cheap."
  [state actions sid idx sd]
  (let [pk (game/current-player state)
        mine? (= pk (:owner sd))
        phase (:phase state)]
    (and mine?
         (or (has-action? actions [:planet-on [sid idx]])
             (has-action? actions [:planet-off [sid idx]])
             (has-action? actions [:sun-outer idx])
             (has-action? actions [:sun-inner idx])
             (has-action? actions [:planet-buy idx])
             (and (:resource sd)
                  (has-action? actions [:planet-build idx (:resource sd)]))
             (and (contains? #{:moving :choose-action-type} phase)
                  ;; sundiver stageable as fly src (there is at least one
                  ;; adjacent space; almost always true)
                  true)
             (and (= :activating phase)
                  (activate-target-for-sid state sid))
             (and (= :link-placement phase)
                  (link-src-legal-here? actions sid))))))

(defn- sundiver-tri
  "One sundiver as a clickable triangle."
  [state actions sid idx sd n-total i]
  (let [[cx cy] (board/space-center sid)
        r 10
        ;; Spread multiple sundivers along a small arc above the space
        span 22
        offset (if (= n-total 1) 0 (- (* (/ i (dec n-total)) span) (/ span 2.0)))
        tcx (+ cx offset)
        tcy (- cy 18)
        pnd @pending
        is-pending? (and pnd (= :sundiver (:kind pnd))
                         (= sid (:sid pnd)) (= idx (:idx pnd)))
        clickable? (sundiver-clickable? state actions sid idx sd)]
    [:g {:key (str "sd-" (pr-str sid) "-" idx)}
     [:polygon {:points (tri-points tcx tcy r)
                :fill (player-color state (:owner sd))
                :stroke (cond
                          is-pending? "#ffe066"
                          clickable?  "#ffffff"
                          :else       "#000")
                :stroke-width (cond is-pending? 2.4 clickable? 1.6 :else 0.8)
                :style {:cursor (if clickable? "pointer" "default")}
                :on-click (fn [e]
                            (.stopPropagation e)
                            (let [ch (sundiver-click-chain state actions sid idx sd)]
                              (when (seq ch) (dispatch-chain! ch))))}]
     (when (:resource sd)
       [:circle {:cx tcx :cy (- tcy 3) :r 3.2
                 :fill (get board/orbit-colors (:resource sd))
                 :stroke "#000" :stroke-width 0.6
                 :style {:pointer-events "none"}}])
     (when (:on-planet? sd)
       [:circle {:cx tcx :cy tcy :r 12
                 :fill "none" :stroke "#ffffff" :stroke-width 1.2
                 :style {:pointer-events "none"}}])]))

(defn- sundivers-at-space [state actions sid]
  (let [divs (game/sundivers-at state sid)
        n (count divs)]
    (into [:g]
      (for [[i sd] (map-indexed vector divs)]
        [sundiver-tri state actions sid i sd n i]))))

(defn- flame-marker
  "Clickable flame — click = commit / advance / decline / done-*."
  [state actions sid]
  (let [[x y] (board/space-center sid)
        ch (flame-click-chain state actions)
        active? (boolean ch)]
    [:g {:style {:cursor (if active? "pointer" "default")}
         :on-click (fn [e]
                     (.stopPropagation e)
                     (when (seq ch) (dispatch-chain! ch)))}
     [:circle {:cx x :cy y :r 20
               :fill "none"
               :stroke board/flame-color
               :stroke-width (if active? 3.0 2.0)
               :opacity (if active? 1.0 0.85)}]
     [:text {:x x :y (+ y 6) :text-anchor "middle"
             :font-size 16 :font-weight "bold"
             :font-family "monospace" :fill board/flame-color
             :style {:pointer-events "none"}} "F"]]))

(defn- link-line [state {:keys [a b owner]}]
  (let [[ax ay] (board/space-center a)
        [bx by] (board/space-center b)]
    [:line {:x1 ax :y1 ay :x2 bx :y2 by
            :stroke (player-color state owner)
            :stroke-width 3
            :opacity 0.9
            :style {:pointer-events "none"}}]))

(defn- pending-link-preview
  "If we're mid link-placement with a src staged, draw a dashed
   preview line to the mouse-hovered cell — or highlight potential dsts
   (handled via space stroke). Just draws src marker for now."
  [state actions]
  (let [pnd @pending]
    (when (and pnd (= :link-src (:kind pnd))
               (= :link-placement (:phase state)))
      (let [[x y] (board/space-center (:sid pnd))]
        [:circle {:cx x :cy y :r 26
                  :fill "none"
                  :stroke "#ffe066"
                  :stroke-width 2
                  :stroke-dasharray "4 4"
                  :style {:pointer-events "none"}}]))))

(defn- resource-token-glyph [sid resources]
  (let [[cx cy] (board/space-center sid)
        n (count resources)]
    (into [:g {:style {:pointer-events "none"}}]
      (for [[i c] (map-indexed vector resources)]
        [:circle {:key (str "res-" (pr-str sid) "-" i)
                  :cx (+ cx (* (- i (/ (dec n) 2.0)) 5))
                  :cy (+ cy 16)
                  :r 3.0
                  :fill (get board/orbit-colors c)
                  :stroke "#000" :stroke-width 0.5}]))))

(defn- link-src-overlay-cached
  "Subtle glow on legal link sources during :link-placement (no src
   pending). Uses precomputed :link-src-sids from `derived`."
  [derived]
  (into [:g]
    (for [sid (:link-src-sids derived)]
      (let [[x y] (board/space-center sid)]
        [:circle {:key (str "linksrc-" (pr-str sid))
                  :cx x :cy y :r 24
                  :fill "none"
                  :stroke "#8fddff"
                  :stroke-width 1.5
                  :opacity 0.7
                  :stroke-dasharray "2 3"
                  :style {:pointer-events "none"}}]))))

(defn- link-placement-click-router
  "When link-placement is active, we override the normal space handler
   to run the src→dst flow. Returns a chain or nil."
  [state actions sid]
  (when (= :link-placement (:phase state))
    (let [pnd @pending]
      (cond
        (nil? pnd)
        (link-src-click-chain state actions sid)

        (and pnd (= :link-src (:kind pnd)))
        (or (link-dst-click-chain actions (:sid pnd) sid)
            ;; Clicking a different valid src reselects it
            (link-src-click-chain state actions sid))))))

(defn- undo-icon
  "Small clickable undo arrow in the top-left of the SVG."
  []
  (let [d (undo-depth)
        enabled? (pos? d)]
    [:g {:style {:cursor (if enabled? "pointer" "default")
                 :opacity (if enabled? 1.0 0.35)}
         :on-click (fn [e]
                     (.stopPropagation e)
                     (when enabled? (dispatch-undo!)))}
     [:circle {:cx 30 :cy 30 :r 22
               :fill "#1a1a2c"
               :stroke "#8fddff"
               :stroke-width 1.6}]
     [:text {:x 30 :y 37 :text-anchor "middle"
             :font-size 22 :font-family "monospace"
             :fill "#8fddff" :font-weight "bold"
             :style {:pointer-events "none"}} "↶"]
     [:text {:x 30 :y 62 :text-anchor "middle"
             :font-size 10 :font-family "monospace"
             :fill "#8fddff" :opacity 0.75
             :style {:pointer-events "none"}} (str d)]]))

(defn board-svg []
  (let [t0 (now-ms)
        state   @game-state
        actions @legal-actions-atom
        pnd     @pending]
    (when state
      (let [derived (derive-clickable state actions pnd)
            t-derive (now-ms)
            link-phase? (= :link-placement (:phase state))
            n (swap! render-counter inc)
            _ (when @profile?
                (js/console.log
                  (str "RENDER#" n " derive=" (.toFixed (- t-derive t0) 1) "ms"
                       " sundivers=" (reduce + (map count (vals (:sundivers state))))
                       " links=" (count (:links state))
                       " cities=" (count (:cities state)))))]
        [:svg {:viewBox (str "0 0 " view-size " " view-size)
               :preserveAspectRatio "xMidYMid meet"
               :style {:width "100%" :height "100%"
                       :display "block"}
               :on-click (fn [_e]
                           (when @pending (reset! pending nil)))}
         [background]
         [beam-line]
         (for [ring board/orbits
               :let [[ri ro] (board/orbit-radii ring)]]
           ^{:key (str "rd-" (name ring))}
           [:g
            [ring-divider ri]
            [ring-divider ro]])
         ;; INLINE (fn ...) instead of Reagent [component ...] form.
         ;; Skipping the Reagent component boundary avoids deep = compares
         ;; of `state` and `actions` per space per render — that was the
         ;; measured cost. Now we just build hiccup directly.
         (for [ring (reverse board/orbits)
               i (range (board/ring-sizes ring))
               :let [sid (board/orbit-space ring i)]]
           (with-meta
             (if link-phase?
               (let [d (cached-orbit-path sid)
                     city (get (:city-by-sid derived) sid)
                     ms-owner (get (:ms-by-sid derived) sid)
                     base (get board/orbit-colors (board/orbit-of sid))
                     highlight? (contains? (:clickable-sids derived) sid)
                     dst?       (contains? (:dst-sids derived) sid)
                     fill (cond
                            ms-owner (player-color state ms-owner)
                            city     (get board/orbit-colors (:color city))
                            :else    base)]
                 [:path {:d d
                         :fill fill
                         :fill-opacity (if city 0.9 0.62)
                         :stroke (cond dst? "#ffe066"
                                       highlight? "#8fddff"
                                       :else "#000")
                         :stroke-width (cond dst? 2.4 highlight? 2.0 :else 0.5)
                         :style {:cursor (if highlight? "pointer" "default")}
                         :on-click (fn [e]
                                     (.stopPropagation e)
                                     (let [ch (link-placement-click-router state actions sid)]
                                       (when (seq ch) (dispatch-chain! ch))))}])
               (orbital-space-component sid state actions derived))
             {:key (str "sp-" (name ring) "-" i)}))
         (for [k (range board/num-wedges)
               :let [sid (board/sun-space k)]]
           (with-meta (wedge-component sid state actions derived)
             {:key (str "w-" k)}))
         (for [[orbit sid] (:planets state)]
           (with-meta (planet-marker state actions orbit sid)
             {:key (str "pl-" (name orbit))}))
         (for [[i link] (map-indexed vector (:links state))]
           (with-meta (link-line state link)
             {:key (str "lk-" i)}))
         (for [[sid c] (:cities state)]
           (with-meta (city-marker sid c)
             {:key (str "ci-" (pr-str sid))}))
         (for [[pk pd] (:players state)
               :when (and (:mothership pd) (not= :supply (:mothership pd)))]
           (with-meta (mothership-marker state pk (:mothership pd))
             {:key (str "ms-" pk)}))
         (for [[sid divs] (:sundivers state)
               :when (seq divs)]
           (with-meta (sundivers-at-space state actions sid)
             {:key (str "sd-" (pr-str sid))}))
         (for [[sid res] (:resources state)
               :when (seq res)]
           (with-meta (resource-token-glyph sid res)
             {:key (str "rt-" (pr-str sid))}))
         (when-let [fs (game/flame-space state)]
           (with-meta (flame-marker state actions fs) {:key "flame"}))
         (when link-phase? (link-src-overlay-cached derived))
         (pending-link-preview state actions)
         (undo-icon)]))))

;; ── Side panels ───────────────────────────────────────────────────────────

(defn- panel-label [s]
  [:div {:style {:color "#5a5a78" :font-size "0.7rem"
                 :letter-spacing "0.1em" :margin "12px 0 4px 0"
                 :text-transform "uppercase"}}
   s])

(defn- chip [bg fg text]
  [:span {:style {:display "inline-block"
                  :padding "2px 6px" :margin "1px 3px 1px 0"
                  :background bg :color fg
                  :border-radius "3px"
                  :font-size "0.72rem"}}
   text])

(defn- prompt-text
  "What is the current player being asked to do, in one line?"
  [state]
  (let [phase (:phase state)
        pd (:phase-data state)]
    (case phase
      :place-mothership       "Click a beam space to place your mothership."
      :resolve-mothership     "Click a highlighted space (or the flame to stay)."
      :choose-action-type     "Click a sundiver to activate, or a launch target to move."
      :moving                 (if (:kind @pending)
                                "Click a destination space."
                                (str (:moves-left pd) " moves left — click a launch target or your sundiver, or click the flame."))
      :activating             (cond
                                (nil? (:target pd))
                                "Click a sundiver to start activating."
                                :else
                                (str "Activating " (name (:target pd))
                                     " (" (:activated-count pd) " done). Click another, or the flame."))
      :activating-sun-space   "Click the inner triangle (place component) or the outer red (return + energy)."
      :activating-planet-space "Click a sundiver: no resource = buy; carrying non-matching = build city."
      :link-placement         (if (:kind @pending)
                                "Click an adjacent destination."
                                (str (:actions-left pd) " link actions left. Click a highlighted source, or the flame."))
      :owner-bonus-decision   "Click the city to take the bonus, or the flame to decline."
      :activator-bonus-decision "Click the city to take the bonus, or the flame to decline."
      :drawing-cards          "Drawing…"
      :orbit-planets          "Orbiting…"
      :advance-mothership     "Advancing…"
      :pass-flame             "Passing flame…"
      :game-over              "Game over."
      "")))

(defn status-header []
  (let [state @game-state pk @player-key cur (game/choice-player state)]
    (when state
      (let [w (:winner state)
            player-col (when cur (player-color state cur))]
        [:div {:style {:padding "14px 16px" :margin "0 0 12px 0"
                       :border-radius "6px"
                       :background "#0e0f18"
                       :border (str "2px solid "
                                    (or player-col "#5a5a78"))}}
         (when w
           [:div {:style {:padding "6px 10px" :margin "0 0 10px 0"
                          :border-radius "4px"
                          :background (cond
                                        (= w :salvation)         "#1f3a3a"
                                        (= (:result w) :win)     "#1f3a1f"
                                        :else                    "#3a1f1f")
                          :color (cond
                                   (= w :salvation)     "#9feeee"
                                   (= (:result w) :win) "#9fee9f"
                                   :else                "#ee9f9f")
                          :font-weight "bold" :font-size "1rem"}}
            (cond
              (= w :salvation)          "SALVATION — communal victory"
              (= (:result w) :win)      (str "VICTORY — " (:winner w))
              (= (:result w) :none)     "NO WINNER — tie cascade")])
         [:div {:style {:color (or player-col "#5a5a78")
                        :font-size "1.7rem" :font-weight "bold"
                        :line-height "1.0" :letter-spacing "0.02em"}}
          (or cur "—")]
         [:div {:style {:color "#c8c8dd" :font-size "1.0rem"
                        :margin "6px 0 4px 0" :font-weight "bold"
                        :text-transform "uppercase" :letter-spacing "0.05em"}}
          (name (:phase state))]
         [:div {:style {:color "#e8e8f8" :font-size "0.95rem"
                        :line-height "1.35"}}
          (prompt-text state)]
         (when (and pk cur (not= pk cur))
           [:div {:style {:color "#ffcc44" :font-size "0.8rem"
                          :margin-top "6px" :font-style "italic"}}
            (str "Waiting on " cur "…")])
         [:div {:style {:color "#5a5a78" :font-size "0.72rem"
                        :margin-top "8px"}}
          (str "turn " (:turn state)
               "  ·  flares " (:flares-drawn state) "/" game/flares-to-end
               "  ·  undo " (undo-depth))]]))))

(defn market-panel []
  (let [state @game-state]
    (when state
      [:div
       [panel-label "MARKET"]
       (for [c board/orbits]
         ^{:key (str "mk-" (name c))}
         [:div {:style {:font-size "0.78rem" :line-height "1.4"
                        :color (get board/orbit-colors c)}}
          (name c) ": "
          (chip "#2a1a1a" (get board/orbit-colors c)
                (str "R " (get-in state [:market-resources c] 0)))
          (chip "#1a2a1a" (get board/orbit-colors c)
                (str "C " (get-in state [:market-cities c] 0)))])
       [:div {:style {:color "#ffcc44" :font-size "0.78rem" :margin-top "6px"}}
        "energy pool: " (:energy-pool state)]])))

(defn players-panel []
  (let [state @game-state
        cur   (game/current-player state)]
    (when state
      [:div
       [panel-label "PLAYERS"]
       (for [pk (:turn-order state)
             :let [pd (get-in state [:players pk])
                   col (player-fill (:wedge-color pd))]]
         ^{:key (str "pl-" pk)}
         [:div {:style {:padding "4px 6px"
                        :margin-bottom "3px"
                        :background (if (= pk cur) "#1f1f33" "#0c0c14")
                        :border-left (str "3px solid " col)}}
          [:div {:style {:color col :font-size "0.85rem"
                         :font-weight (if (= pk cur) "bold" "normal")}}
           pk (when (= pk cur) " <")]
          [:div {:style {:color "#5a5a78" :font-size "0.7rem"}}
           (str "E:" (:energy pd) " H:" (:habitat pd)
                " R:" (:reserve pd) " C:" (:components pd)
                " P:" (:city-platforms pd) " L:" (:links-supply pd)
                " V:" (:vaporized pd))]])])))

(defn hand-panel []
  (let [state @game-state pk @player-key]
    (when (and state pk)
      (let [hand (get-in state [:hands pk] [])]
        [:div
         [panel-label (str "HAND (" (count hand) ")")]
         (if (empty? hand)
           [:span {:style {:color "#444" :font-size "0.75rem"}} "-"]
           (for [[i c] (map-indexed vector hand)
                 :let [col (if (game/flare-card? c) "#ff8844"
                             (get board/orbit-colors (:suit c)))]]
             ^{:key (str "h-" i)}
             [:span {:style {:display "inline-block"
                             :padding "2px 5px" :margin "1px 2px"
                             :background "#1a1a2a"
                             :border (str "1px solid " col)
                             :border-radius "3px"
                             :color col
                             :font-size "0.7rem"}}
              (str (name (:suit c)) " " (:value c))]))]))))

(defn solar-panel []
  (let [state @game-state]
    (when state
      [:div
       [panel-label "SOLAR NETWORK"]
       (for [k (range board/num-wedges)
             :let [c (board/wedge-color k)
                   sn (get-in state [:solar-network k])
                   act (apply + (vals (:active sn)))
                   exh (apply + (vals (:exhausted sn)))]]
         ^{:key (str "sn-" k)}
         [:div {:style {:font-size "0.78rem" :line-height "1.4"
                        :color (get board/orbit-colors c)}}
          (str "wedge " k " (" (name c) "): "
               act " active, " exh " spent")])])))

(defn log-panel []
  (let [entries @action-log
        recent  (vec (take-last 30 entries))]
    [:div
     [panel-label "LOG"]
     [:div {:style {:max-height "200px" :overflow-y "auto"
                    :font-size "0.72rem"}}
      (for [[i e] (map-indexed vector (reverse recent))]
        ^{:key (str "log-" (- (count entries) i))}
        [:div {:style {:padding "1px 0"}}
         [:span {:style {:color "#444"}} (str "T" (:turn e) " ")]
         [:span {:style {:color (player-fill
                                  (get-in @game-state [:players (:player e) :wedge-color]))}}
          (:player e)]
         [:span {:style {:color "#444"}} (str " [" (name (or (:phase e) "")) "] ")]
         [:span {:style {:color "#aabbcc"}} (:action e)]])]]))

;; ── Live game view ────────────────────────────────────────────────────────

(defn game-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#06070d" :color "#ccbbee"
                 :font-family "monospace"}}
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "6px"
                  :min-width 0}}
    [board-svg]]
   [:div {:style {:width "340px" :padding "12px" :overflow-y "auto"
                  :border-left "1px solid #161620" :flex-shrink 0}}
    [status-header]
    [players-panel]
    [market-panel]
    [solar-panel]
    [hand-panel]
    [log-panel]]])

;; ── Create view ───────────────────────────────────────────────────────────

(defn create-view []
  [components/create-lobby
   {:game-type      "future"
    :title          "FUTURE — New Game"
    :current-player (when (and (exists? js/playerKey)
                               (not (str/blank? js/playerKey))
                               (not= "--observer--" js/playerKey))
                      js/playerKey)
    :min-players    2
    :max-players    5
    :accent         "#ff8844"
    :slot-bg        "#1a1a2c"
    :background     "#06070d"}])

;; ── Observe list view ────────────────────────────────────────────────────

(defn observe-view []
  (let [raw  (when (exists? js/observeGames) js/observeGames)
        list (when raw (safe-read raw))]
    [:div {:style {:padding "32px" :background "#06070d"
                   :color "#ccbbee" :font-family "monospace" :min-height "100vh"}}
     [:h2 {:style {:color "#ff8844" :margin-bottom "18px"}} "OBSERVE FUTURE GAMES"]
     (if (seq list)
       (for [g list]
         ^{:key (str (:key g))}
         [:a {:href (str "/future/play/" (:key g))
              :style {:display "block" :padding "10px 14px" :margin "4px 0"
                      :color "#ccbbee" :background "#1a1a2c"
                      :border "1px solid #4a3a6a" :border-radius "3px"
                      :text-decoration "none"}}
          (str (:key g) " — players: " (str/join ", " (:players g)))])
       [:div {:style {:color "#5a5a78"}} "(no live future games)"])]))

;; ── Local bot simulator (/future/generate) ────────────────────────────────

(def default-bot-players ["Sola" "Vega" "Lyra" "Nova" "Pyre"])

(def terminal-choice-keys
  #{[:done-moving] [:done-activating] [:done-linking]
    [:done-activating-space]
    [:decline-bonus] [:no-activation-possible]})

(defn- ck-planet-on? [[ck _]]
  (and (vector? ck) (= :planet-on (first ck))))

(defn- ck-planet-off? [[ck _]]
  (and (vector? ck) (= :planet-off (first ck))))

(defn bot-pick
  "Bot policy: auto-board planets when possible, never disembark, avoid
   :done-* while other actions are available."
  [actions]
  (let [entries (vec actions)
        boards  (filterv ck-planet-on? entries)]
    (cond
      (seq boards)
      (rand-nth boards)

      :else
      (let [non-term (filterv (fn [e]
                                (and (not (contains? terminal-choice-keys (first e)))
                                     (not (ck-planet-off? e))))
                              entries)
            pool (if (seq non-term)
                   non-term
                   (or (seq (filterv (fn [e] (not (contains? terminal-choice-keys (first e)))) entries))
                       entries))]
        (rand-nth pool)))))

(defonce gen-running    (r/atom false))
(defonce gen-interval-ms (r/atom 250))
(defonce gen-timer      (atom nil))
(defonce gen-num-players (r/atom 3))

(defn- gen-step! []
  (let [state @game-state]
    (when (and state (not= :game-over (:phase state)))
      (let [actions (game/legal-actions state)]
        (when (seq actions)
          (let [[ak thunk] (bot-pick actions)
                nxt (game/force-choice thunk)]
            (push-local-history! state)
            (push-log! ak nxt)
            (reset! game-state nxt)
            (reset! legal-actions-atom (game/legal-actions nxt))))))))

(defn- gen-stop! []
  (reset! gen-running false)
  (when-let [t @gen-timer]
    (js/clearInterval t)
    (reset! gen-timer nil)))

(defn- gen-start! []
  (gen-stop!)
  (reset! gen-running true)
  (reset! gen-timer
          (js/setInterval
            (fn []
              (if (or (not @gen-running) (= :game-over (:phase @game-state)))
                (gen-stop!)
                (gen-step!)))
            @gen-interval-ms)))

(defn- gen-new! []
  (gen-stop!)
  (reset! action-log [])
  (reset! local-history [])
  (reset! pending nil)
  (let [n  (max 2 (min 5 @gen-num-players))
        ps (vec (take n default-bot-players))
        st (game/create-game ps)]
    (reset! game-state st)
    (reset! legal-actions-atom (game/legal-actions st))
    (reset! player-key (first ps))
    (reset! mode-atom :local)))

(defn gen-controls []
  (let [st @game-state run? @gen-running done? (and st (= :game-over (:phase st)))]
    [:div
     [:div {:style {:display "flex" :gap "6px" :flex-wrap "wrap"
                    :align-items "center" :margin-bottom "8px"}}
      [:label {:style {:color "#5a5a78" :font-size "0.72rem"}} "players:"]
      [:input {:type "number" :min 2 :max 5
               :value @gen-num-players
               :on-change (fn [e]
                            (reset! gen-num-players
                                    (js/parseInt (-> e .-target .-value))))
               :style {:width "44px" :padding "4px 6px"
                       :background "#111" :color "#ccbbee"
                       :border "1px solid #334" :border-radius "3px"
                       :font-family "monospace"}}]
      [:button {:on-click gen-new!
                :style {:padding "6px 10px" :background "#2a2a44"
                        :color "#ccbbee" :border "1px solid #4a3a6a"
                        :border-radius "3px" :cursor "pointer"
                        :font-family "monospace"}}
       "New game"]
      [:button {:on-click gen-step! :disabled (or run? done?)
                :style {:padding "6px 10px" :background "#2a443a"
                        :color "#88ddaa" :border "1px solid #4a6a5a"
                        :border-radius "3px" :cursor "pointer"
                        :font-family "monospace"
                        :opacity (if (or run? done?) 0.4 1)}}
       "Step"]
      (if run?
        [:button {:on-click gen-stop!
                  :style {:padding "6px 10px" :background "#442a2a"
                          :color "#ff9999" :border "1px solid #6a4a4a"
                          :border-radius "3px" :cursor "pointer"
                          :font-family "monospace"}}
         "Stop"]
        [:button {:on-click gen-start! :disabled (or done? (nil? st))
                  :style {:padding "6px 10px" :background "#2a3a44"
                          :color "#88bbdd" :border "1px solid #4a5a6a"
                          :border-radius "3px" :cursor "pointer"
                          :font-family "monospace"
                          :opacity (if (or done? (nil? st)) 0.4 1)}}
         "Auto"])
      [:button {:on-click #(dispatch-undo!)
                :disabled (zero? (count @local-history))
                :style {:padding "6px 10px" :background "#2a3a44"
                        :color "#8fddff" :border "1px solid #4a5a6a"
                        :border-radius "3px" :cursor "pointer"
                        :font-family "monospace"
                        :opacity (if (zero? (count @local-history)) 0.4 1)}}
       "Undo ↶"]]
     [:div {:style {:display "flex" :align-items "center" :gap "6px"}}
      [:span {:style {:color "#5a5a78" :font-size "0.75rem"}} "speed:"]
      [:input {:type "range" :min 40 :max 1200 :step 20
               :value @gen-interval-ms
               :on-change (fn [e]
                            (let [v (js/parseInt (-> e .-target .-value))]
                              (reset! gen-interval-ms v)
                              (when run? (gen-start!))))
               :style {:width "120px"}}]
      [:span {:style {:color "#5a5a78" :font-size "0.75rem"}}
       (str @gen-interval-ms "ms")]]]))

(defn generate-view []
  [:div {:style {:display "flex" :flex-direction "row" :height "100vh"
                 :background "#06070d" :color "#ccbbee"
                 :font-family "monospace"}}
   [:div {:style {:flex 1 :display "flex" :justify-content "center"
                  :align-items "center" :padding "6px"
                  :min-width 0}}
    [board-svg]]
   [:div {:style {:width "340px" :padding "12px" :overflow-y "auto"
                  :border-left "1px solid #161620" :flex-shrink 0}}
    [:h3 {:style {:color "#ff8844" :margin "0 0 8px 0"}} "GENERATE"]
    [gen-controls]
    (when @game-state
      [:div
       [status-header]
       [players-panel]
       [market-panel]
       [solar-panel]
       [hand-panel]
       [log-panel]])]])

;; ── Top-level dispatcher ─────────────────────────────────────────────────

(defn page []
  (let [pk          (when (exists? js/playKey) js/playKey)
        is-create?  (and (exists? js/isCreate) js/isCreate)
        is-observe? (and (exists? js/isObserve) js/isObserve)
        is-gen?     (and (exists? js/isGenerate) js/isGenerate)]
    (cond
      is-gen?     [generate-view]
      is-create?  [create-view]
      is-observe? [observe-view]
      (and pk (not (str/blank? pk))) [game-view]
      :else
      [:div {:style {:padding "48px" :color "#5a5a78"
                     :font-family "monospace" :background "#06070d"
                     :min-height "100vh"}}
       "Loading future…"])))

;; ── Keyboard shortcuts ───────────────────────────────────────────────────

(defn- on-key [e]
  (case (.-key e)
    "Escape" (reset! pending nil)
    "u"      (dispatch-undo!)
    "U"      (dispatch-undo!)
    nil))

(defonce _key-listener-installed
  (do (.addEventListener js/window "keydown" on-key)
      true))

;; ── Mount ────────────────────────────────────────────────────────────────

(defn mount-components
  "Render the top-level page and wire up the WebSocket / generator per
   the JS globals set in the HTML shell. Called on init and after every
   hot reload."
  []
  (when-let [el (.getElementById js/document "future")]
    (rdom/render [page] el))
  (let [pk         (when (exists? js/playKey) js/playKey)
        is-create? (and (exists? js/isCreate)   js/isCreate)
        is-observe?(and (exists? js/isObserve)  js/isObserve)
        is-gen?    (and (exists? js/isGenerate) js/isGenerate)]
    (when (and pk (not (str/blank? pk))
               (not is-create?) (not is-observe?) (not is-gen?))
      (reset! mode-atom :ws)
      (reset! player-key (when (exists? js/playerKey) js/playerKey))
      (connect-ws! pk))
    (when is-gen? (gen-new!))))

(defn init!
  "shadow-cljs entrypoint."
  []
  (ajax/load-interceptors!)
  (mount-components))
