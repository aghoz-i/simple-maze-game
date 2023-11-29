module MazeDrawer where

import MazeGenerator
import Graphics.Gloss

drawCell :: Cell -> Float -> Picture
drawCell c cl = translate (x*cl) (y*cl*(-1.0)) $ pictures [drawWallU, drawWallR, drawWallD, drawWallL]
    where   x = fromIntegral (posX (location c))::Float
            y = fromIntegral (posY (location c))::Float
            -- drawWallU   = line [(0,0), (0,0)]
            drawWallU   | (wallU c) = line [(0, cl), (cl, cl)]
                        | otherwise = line [(0,0), (0,0)]
            drawWallR   | (wallR c) = line [(cl, 0), (cl, cl)]
                        | otherwise = line [(0,0), (0,0)]
            drawWallD   | (wallD c) = line [(0, 0), (cl, 0)]
                        | otherwise = line [(0,0), (0,0)]
            drawWallL   | (wallL c) = line [(0, 0), (0, cl)]
                        | otherwise = line [(0,0), (0,0)]

drawMaze :: Maze -> Float -> Picture
drawMaze m cl = foldl (\drew next -> pictures [drew, drawCell next cl]) blank (cells m)

