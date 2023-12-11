module Main where

import MazeGenerator
import GameState
import Drawer

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Display

window :: Display
window = InWindow "Maze Game" (1366, 768) (277,156)

background :: Color
background = white

drawWorld :: GameState -> Picture
drawWorld gameState = pictures $ [gameDrawn, textDrawn]
    where   mazeDrawn = drawMaze (maze gameState) 
            goalDrawn = drawGoalColor (maze gameState)
            positionDrawn = drawPlayer (position gameState) (maze gameState)
            gameDrawn = moveToCenter (maze gameState) $ pictures[goalDrawn, mazeDrawn, positionDrawn]
            initialStateText = drawInitialStateText (isInitialState gameState)
            gameoverText = drawGameoverText (isGameOver gameState)
            exitText = drawExitText
            textDrawn = pictures $ [initialStateText, gameoverText, exitText]
            
inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeyUp) Down _ _)     gameState = move gameState UP
inputHandler (EventKey (SpecialKey KeyDown) Down _ _)   gameState = move gameState DOWN
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _)   gameState = move gameState LEFT
inputHandler (EventKey (SpecialKey KeyRight) Down _ _)  gameState = move gameState RIGHT

inputHandler (EventKey (SpecialKey KeySpace) Down _ _)  gameState 
    | isInitialState gameState = GameState (position gameState) (maze gameState) False (isGameOver gameState)
    | isGameOver gameState = createLevel (isInitialState gameState) nextLevelSize
    | otherwise = gameState
    where   nextLevelSize = (width $ mazeSize $ maze gameState) + 1

inputHandler (EventKey (Char 'r') Down _ _)  gameState 
    | isGameOver gameState = GameState (Location 0 0) (maze gameState) False False
    | otherwise = gameState

inputHandler _ world = world

updateWorld :: Float -> GameState -> GameState
updateWorld _ gameState
    | isGameOver gameState = gameState
    | isAtGoal gameState = GameState (position gameState) (maze gameState) (isInitialState gameState) True
    | otherwise = gameState

main :: IO ()
main = play window background 10 (createLevel True 5) drawWorld inputHandler updateWorld
