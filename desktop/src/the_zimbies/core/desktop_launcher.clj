(ns the-zimbies.core.desktop-launcher
  (:require [the-zimbies.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. the-zimbies-game "The Zimbies" 800 600)
  (Keyboard/enableRepeatEvents true))
