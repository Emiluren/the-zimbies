(ns the-zimbies.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer :all]
            [play-clj.repl :reger :all]))

;; to restart game
;; (in-ns 'the-zimbies.core)
;; (on-gl (set-screen! the-zimbies-game main-screen))

;; useful for repl debugging
;; (e main-screen)
;; (e! identity main-screen :x 200 :y 200)

(def ^:const pixels-per-tile 32)

(def zombie-names ["Joe" "Bob" "Jack" "Mike" "Tom" "Dave" "Paul" "John" "Mark"
                   "Andrew" "Richard" "Peter" "Susan" "Christine" "Janet" "Margaret"
                   "Sarah" "Claire" "Mary" "Hannah" "Sophie" "Laura" "Dorothy" "Joyce"])

(defn create-rect-body!
  [screen width height]
  (let [body (add-body! screen (body-def :static))]
    (->> (polygon-shape :set-as-box (/ width 2) (/ height 2))
         (fixture-def :density 1 :shape)
         (body! body :create-fixture))
    body))

(defn create-character-body!
  [screen radius]
  (let [body (add-body! screen (body-def :dynamic :fixed-rotation true))]
    (->> (circle-shape :set-radius radius)
         (fixture-def :density 1 :shape)
         (body! body :create-fixture))
    body))

(defn create-rect-entity! [screen block width height]
  (assoc block
         :body (create-rect-body! screen width height)
         :width width :height height))

(defn make-block [screen x y img]
  (doto (create-rect-entity! screen img 1 1)
    (body-position! x y 0)))

(defn entity-width [entity]
  (/ (texture! entity :get-region-width) pixels-per-tile))

(defn entity-height [entity]
  (/ (texture! entity :get-region-height) pixels-per-tile))

(defn load-texture [filename]
  (let [tex (texture filename)]
    (assoc tex
           :width (entity-width tex)
           :height (entity-height tex))))

(defn key-code-to-dir [kc]
  (cond
    (= (key-code :dpad-up) kc) :up
    (= (key-code :dpad-left) kc) :left
    (= (key-code :dpad-right) kc) :right
    (= (key-code :dpad-down) kc) :down
    :else nil))

(def speed 4)

(defn move [character]
  (let [last-key (first (:pressed-keys character))]
    (if (nil? last-key)
      (doto character
        (body! :set-linear-velocity 0 0))
      (case last-key
        :up (doto character (body! :set-linear-velocity 0 speed))
        :down (doto character (body! :set-linear-velocity 0 (- speed)))
        :left (doto character (body! :set-linear-velocity (- speed) 0))
        :right (doto character (body! :set-linear-velocity speed 0))
        character))))

(defn copy-position [character body]
  (let [position (body! body :get-position)
        [offset-x offset-y] (:physics-offset character)
        x (+ (.-x position) offset-x)
        y (+ (.-y position) offset-y)]
    (assoc character :x x :y y)))

(defn animate [{:keys [standing walk-left pressed-keys] :as character}
               body screen]
  (-> character
      (copy-position body)
      (merge
       (if (empty? pressed-keys)
         standing
         (animation->texture screen walk-left)))))

(defn set-angle [character dir]
  (let [new-angle (case dir
                    :left 0
                    :right 180
                    :up 270
                    :down 90
                    (:angle character))]
    (assoc character :angle new-angle)))

(defn create-character [filename id frames screen debug-texture]
  (let [sheet (texture filename)
        sheet-width (texture! sheet :get-region-width)
        sheet-height (texture! sheet :get-region-height)
        tiles (texture! sheet :split (/ sheet-width frames) sheet-height)
        character-images (for [n (range frames)]
                          (texture (aget tiles 0 n)))
        standing (second character-images)
        physics-offset [(/ (- 1 (entity-width standing)) 2)
                        (/ (- 1 (entity-height standing)) 2)]
        character (assoc standing
                         :id id
                         :standing standing
                         :walk-left (animation 0.2 character-images)
                         :width (entity-width standing)
                         :height (entity-height standing)
                         :character? true
                         :pressed-keys ()
                         :physics-offset physics-offset
                         :angle 0)]
    [(doto (assoc debug-texture
                  :id id
                  :character-body? true
                  :width 1, :height 1,
                  :character-body? true
                  :pressed-keys ()
                  :invisible? true
                  :body (create-character-body! screen 0.45))
       (body-position! 3 10 0))
     character]))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :camera (orthographic)
                          :world (box-2d 0 0)
                          :renderer (stage))
          game-w (/ (game :width) pixels-per-tile)
          block-img (texture "Block.gif")
          sand (texture "ground.png")]
      (width! screen game-w)
      [(for [x (range 25)
             y (range 15)]
         (assoc sand :width 1 :height 1 :x x :y (+ y 3)))
       (for [[x y] [[3 0] [4 0] [5 0] [6 1] [3 2] [4 2] [6 2] [3 3] [3 4]]]
         (make-block screen x (+ y 3) block-img))
       (create-character "Explorer_walking.png" :explorer 4 screen block-img)]))

  :on-key-down
  (fn [screen entities]
    ;; Add the pressed key to the explorer's list if it's not already pressed
    (for [entity entities]
      (let [new-key (key-code-to-dir (:key screen))]
        (if (and new-key
                 (= :explorer (:id entity))
                 (not (some #{new-key} (:pressed-keys entity))))
          (-> entity
              (update :pressed-keys conj new-key)
              (set-angle new-key))
          entity))))

  :on-key-up
  (fn [screen entities]
    ;; Remove the pressed key from the explorer's list
    (for [entity entities]
      (if (= :explorer (:id entity))
        (let [released (key-code-to-dir (:key screen))]
          (-> entity
              (update :pressed-keys #(remove #{released} %))
              (as-> updated
                  (set-angle updated (first (:pressed-keys updated))))))
        entity)))

  :on-render
  (fn [screen entities]
    (clear!)
    (let [all-character-bodies (filter :character-body? entities)
          character-map (reduce (fn [m b] (assoc m (:id b) b)) {}
                                all-character-bodies)
          entities (->> (for [entity entities]
                          (cond (:character? entity)
                                (animate entity ((:id entity) character-map) screen)
                                (:character-body? entity)
                                (move entity)
                                :else entity))
                        (step! screen))
          visible-entities (filter (complement :invisible?) entities)]
      (render! screen visible-entities)
      entities)))

(defgame the-zimbies-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!)))

(set-screen-wrapper!
 (fn [screen screen-fn]
   (try (screen-fn)
        (catch Exception e
          (println "OH GOD! SO MUCH BLOOD!")
          (println (.getMessage e))
          (.printStackTrace e)
          (set-screen! the-zimbies-game blank-screen)))))
