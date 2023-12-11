module GameState where

import MazeGenerator
import Data.Map as Map
import System.Random

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord)
data GameState = GameState { position :: Location
                           , maze :: Maze
                           , isInitialState :: Bool
                           , isGameOver :: Bool}

directionVectorMap = Map.fromList $ zip [UP, DOWN, LEFT, RIGHT] 
                                        [(0, (-1)), (0, 1), ((-1), 0), (1, 0)]

move :: GameState -> Direction -> GameState
move gs dir
    | isGameOver gs || isInitialState gs= gs
    | dir == UP && wallU currentCell    = gs
    | dir == DOWN && wallD currentCell  = gs
    | dir == LEFT && wallL currentCell  = gs
    | dir == RIGHT && wallR currentCell = gs
    | otherwise                         = GameState newPosition (maze gs) (isInitialState gs) (isGameOver gs) 
     where  lastPosition = position gs
            currentCell = getCellOnLocation (maze gs) lastPosition
            newPosition = Location ((posX lastPosition) + (fst $ directionVectorMap ! dir)) ((posY lastPosition) + (snd $ directionVectorMap ! dir))

getCellOnLocation :: Maze -> Location -> Cell
getCellOnLocation m l = cellList !! index
    where   cellList = cells m
            x = posX l
            y = posY l
            mHeight = height $ mazeSize m
            index = y * mHeight + x

createLevel :: Bool -> Int -> GameState
createLevel initial size = GameState (Location 0 0) (generateMaze g $ initializeMaze ms) initial False
    where   ms = MazeSize size size
            g = Generator ms [Location 0 0] [Location 0 0] $ mkStdGen 100

isAtGoal :: GameState -> Bool
isAtGoal gs
    | x == xGoal && y == yGoal  = True
    | otherwise                 = False
    where   x = posX $ position gs
            y = posY $ position gs
            xGoal = (width $ mazeSize $ maze gs) - 1
            yGoal = (height $ mazeSize $ maze gs) - 1