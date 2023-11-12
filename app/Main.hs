module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Maze Game" (1366, 768) (277,156)

background :: Color
background = white

type World = (Float, Float, Float)

drawWorld :: World -> Picture
drawWorld (x, y, state) 
    | even (floor (state)) = translate (x) (y) $ circleSolid 10
    | otherwise = translate (x) (y) $ circle 10

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y, state)    = (x      , y + 20 , state)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y, state)  = (x      , y - 20 , state)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y, state)  = (x - 20 , y      , state)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y, state) = (x + 20 , y      , state)
inputHandler _ world = world

updateWorld :: Float -> World -> World
updateWorld _ (x, y, 1.0) = (x, y, 0.0)
updateWorld _ (x, y, 0.0) = (x, y, 1.0)

main :: IO ()
main = play window background 2 (0, 0, 0) drawWorld inputHandler updateWorld
