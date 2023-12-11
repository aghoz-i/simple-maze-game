module Drawer where

import MazeGenerator
import Graphics.Gloss

moveToCenter :: Maze -> Picture -> Picture
moveToCenter m = translate ((fromIntegral cols::Float)*cw*(-0.5)) ((fromIntegral (rows-2)::Float)*cw*0.5)
    where   cols = width $ mazeSize m
            rows = height $ mazeSize m
            cw = getCellWidth m

drawCell :: Cell -> Float -> Picture
drawCell c cw = translate (x*cw) (y*cw*(-1.0)) $ pictures [drawWallU, drawWallR, drawWallD, drawWallL]
    where   x = fromIntegral (posX (location c))::Float
            y = fromIntegral (posY (location c))::Float
            drawWallU   | (wallU c) = line [(0, cw), (cw, cw)]
                        | otherwise = line [(0,0), (0,0)]
            drawWallR   | (wallR c) = line [(cw, 0), (cw, cw)]
                        | otherwise = line [(0,0), (0,0)]
            drawWallD   | (wallD c) = line [(0, 0), (cw, 0)]
                        | otherwise = line [(0,0), (0,0)]
            drawWallL   | (wallL c) = line [(0, 0), (0, cw)]
                        | otherwise = line [(0,0), (0,0)]

drawMaze :: Maze -> Picture
drawMaze m = foldl (\drew next -> pictures [drew, drawCell next cw]) blank (cells m)
    where   cw = getCellWidth m

drawGoalColor :: Maze -> Picture
drawGoalColor m = translate (xGoal*cw + cw*0.5) (yGoal*cw*(-1.0) + cw*0.5) $ color green $ rectangleSolid cw cw
    where   xGoal = fromIntegral ((width $ mazeSize m) - 1)::Float
            yGoal = fromIntegral ((height $ mazeSize m) - 1)::Float
            cw = getCellWidth m

drawPlayer :: Location -> Maze -> Picture
drawPlayer l m = translate (x*cw + cw*0.5) (y*cw*(-1.0) + cw*0.5) $ circleSolid (cw*0.40)
    where   x = fromIntegral (posX l)::Float
            y = fromIntegral (posY l)::Float
            cw = getCellWidth m

drawGameoverText :: Bool -> Picture
drawGameoverText isGameOver
    | isGameOver =  pictures $ [ color blue $ 
                    translate (-300) (30) $ 
                    scale 0.5 0.5 $ 
                    text "CONGRATULATIONS!"
                    ,  color blue $ 
                    translate (-250) (-20) $ 
                    scale 0.2 0.2 $ 
                    text "Press SPACE to go to the next level."
                    ,  color blue $ 
                    translate (-170) (-70) $ 
                    scale 0.2 0.2 $ 
                    text "or Press r to try again." ]
    | otherwise = blank

drawInitialStateText :: Bool -> Picture
drawInitialStateText isInitialState
    | isInitialState =  pictures $ [ color blue $ 
                        translate (-340) (15) $ 
                        scale 0.5 0.5 $ 
                        text "MAZE PUZZLE GAME"
                        ,  color blue $ 
                        translate (-240) (-35) $ 
                        scale 0.2 0.2 $ 
                        text "Press SPACE to start a new game!" ] 
    | otherwise = blank

drawExitText :: Picture
drawExitText =  color red $ 
                translate (-670) (350) $ 
                scale 0.15 0.15 $ 
                text "Press ESC to exit" 

getCellWidth :: Maze -> Float
getCellWidth m = 500/rows
    where   rows = fromIntegral (height $ mazeSize m)::Float