module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Maze Game" (1366, 768) (277,156)

background :: Color
background = white

title :: Picture 
title = translate (-300) (-100) (text "Maze Game")

main :: IO ()
main = display window background title
