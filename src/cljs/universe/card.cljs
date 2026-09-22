(ns universe.card
  "Drawing a UNIVERSE card in the browser.

   None of the geometry lives here.  The rosette packing, the ring radii and
   the back's emblem are worked out by universe/deck.py for the printed deck,
   and universe/make_web.py emits them as `/universe/deck.json` -- so this
   namespace only places what it is told to, and the card on screen is the
   card that comes back from the printer.  Regenerate with `make web` in
   universe/ if the deck ever changes.

   Everything is drawn in cut-card units, 750 x 1050, and scaled by the
   viewBox, so a card is crisp at any size."
  (:require
   [reagent.core :as r]
   [universe.deck :as deck]))

(defonce geometry (r/atom nil))

(defn load!
  "Fetch the deck geometry once.  Until it arrives cards draw as blanks, which
   is what the table looks like before the first deal anyway."
  []
  (when (nil? @geometry)
    (-> (js/fetch "/universe/deck.json")
        (.then #(.json %))
        (.then #(reset! geometry (js->clj % :keywordize-keys true)))
        (.catch #(js/console.error "could not load the deck geometry" %)))))

(defn- shape-path
  "`load!` keywordizes the JSON, so the geometry is keyed by keyword even where
   the file has strings -- `(keyword (name …))` accepts a shape either way."
  [g shape]
  (get-in g [:paths (keyword (name shape))]))

(defn- ink [g color]
  (get-in g [:colors (keyword (name color))]))

;; ── The face ───────────────────────────────────────────────────────────────

(defn- rosette
  "The middle of the card: `n` copies of the shape on a ring, each turned by
   its own share of a full circle, scaled so the outermost ink touches the same
   circle whatever the shape or the count."
  [g shape n ink]
  (let [{:keys [places reach]} (get-in g [:rosettes (keyword (str (name shape) ":" n))])
        {:keys [cx cy d]}      (:field g)
        s (/ (/ d 2.0) reach)]
    (into [:g]
          (for [[i [phi ox oy]] (map-indexed vector places)]
            ^{:key i}
            [:path {:d (shape-path g shape)
                    :fill ink
                    :transform (str "translate(" (+ cx (* ox s)) "," (+ cy (* oy s)) ") "
                                    "rotate(" phi ") scale(" s ")")}]))))

(defn- index-block
  "The number with its shape beneath it.  `turn` is 0 for the top-left copy and
   180 for the one in the opposite corner, which is what lets the card be read
   either way up."
  [g shape n ink turn]
  (let [{:keys [x numberBaseline numberCap glyphCx glyphCy glyphR]} (:corner g)
        {:keys [cx cy]} (:field g)]
    [:g {:transform (str "rotate(" turn " " cx " " cy ")")}
     [:text {:x x :y numberBaseline
             :text-anchor "middle"
             :fill ink
             :font-family "URW Gothic, Century Gothic, Questrial, Inter, sans-serif"
             :font-weight 600
             :font-size (* numberCap 1.38)}
      n]
     [:path {:d (shape-path g shape)
             :fill ink
             :transform (str "translate(" glyphCx "," glyphCy ") scale(" glyphR ")")}]]))

(defn face
  "One card, face up."
  [card]
  (let [g @geometry]
    (when g
      (let [shape (deck/shape card)
            n     (deck/number card)
            paint (ink g (deck/color card))]
        [:g
         [:rect {:x 0 :y 0 :width 750 :height 1050 :rx 38 :fill "#ffffff"}]
         [rosette g shape n paint]
         [index-block g shape n paint 0]
         [index-block g shape n paint 180]]))))

;; ── The back ───────────────────────────────────────────────────────────────

(defn back
  "Two concentric rings on a violet-black ground.  Turn it end over end and
   every symbol lands on a copy of itself in the same color, so there is no
   telling which way up a face-down card is being held."
  []
  (let [g @geometry]
    (when-let [b (:back g)]
      [:g
       [:rect {:x 0 :y 0 :width 750 :height 1050 :rx 38 :fill (:ground b)}]
       (let [{:keys [cx cy r width color]} (:ring b)]
         [:circle {:cx cx :cy cy :r r :fill "none" :stroke color :stroke-width width}])
       (into [:g]
             (for [[i {:keys [shape color phi x y r]}] (map-indexed vector (:marks b))]
               ^{:key i}
               [:path {:d (shape-path g shape)
                       :fill (ink g color)
                       :transform (str "translate(" x "," y ") rotate(" phi ") scale(" r ")")}]))])))

;; ── What the table actually calls ──────────────────────────────────────────

(defn card
  "A card at `width` pixels, face up when `card` is a number and face down when
   it is nil -- which is exactly how a hidden hand arrives from the server:
   the cards are not there to draw."
  [{:keys [value width dim? style]}]
  (let [w (or width 84)]
    [:svg {:viewBox "0 0 750 1050"
           :width w
           :height (* w 1.4)
           :style (merge {:display "block"
                          :border-radius (str (* w 0.05) "px")
                          :box-shadow "0 2px 8px rgba(0,0,0,0.55)"
                          :opacity (if dim? 0.45 1)}
                         style)}
     (if value [face value] [back])]))

(defn hand
  "A row of cards, overlapping slightly the way a hand is held."
  [{:keys [cards width dim?]}]
  (let [w (or width 84)]
    (into [:div {:style {:display "flex" :gap "4px"}}]
          (for [[i c] (map-indexed vector cards)]
            ^{:key i} [card {:value c :width w :dim? dim?}]))))
