module MazeGenerator where

import System.Random

data MazeSize = MazeSize    { width :: Int
                            , height:: Int } deriving Show

data Location = Location    { posX::Int
                            , posY::Int } deriving (Show, Eq)

data Cell = Cell    { location::Location
                    , wallU::Bool
                    , wallR::Bool
                    , wallD::Bool
                    , wallL::Bool } deriving Show

data Maze = Maze { cells::[Cell] } deriving Show

data Generator = Generator  { mazeSize::MazeSize
                            , visited::[Location]
                            , stack::[Location] 
                            , seed::StdGen} deriving Show

initializeCells :: MazeSize -> Int -> [Cell] -> [Cell]
initializeCells ms new cur
    | (width ms) * (height ms) == (length cur) = cur
    | otherwise = initializeCells ms (new+1) $ cur ++    [Cell   { location = Location   { posX = new `mod` (width ms)
                                                                                        , posY = new `div` (width ms) }
                                                                , wallU = True
                                                                , wallR = True
                                                                , wallD = True
                                                                , wallL = True } ]

initializeMaze :: MazeSize -> Maze
initializeMaze ms = Maze {cells = initializeCells ms 0 []}

generateMaze :: Generator -> Maze -> Maze
generateMaze g m
    | allCellsVisited   = m
    | needToPopStack    = generateMaze (Generator {mazeSize = mazeSize g, visited = visited g, stack = tail (stack g), seed = newSeed}) m
    | otherwise         = generateMaze (Generator {mazeSize = mazeSize g, visited = nextCellLocation:visited g, stack = nextCellLocation:stack g, seed = newSeed}) newMaze
    where   allCellsVisited = (width (mazeSize g)) * (height (mazeSize g)) == (length $ visited g)
            currentCellLocation = head $ stack g
            neighbours = getUnvisitedNeighbours (mazeSize g) currentCellLocation (visited g) 
            needToPopStack = length neighbours == 0
            (nextCellLocationIndex, newSeed) = randomR (0, (length neighbours) -1) $ seed g
            nextCellLocation = neighbours !! nextCellLocationIndex
            newMaze = openAPathInMaze (mazeSize g) nextCellLocation currentCellLocation m
            
getUnvisitedNeighbours :: MazeSize -> Location -> [Location] -> [Location]
getUnvisitedNeighbours ms curr visited = [z | z <- neighbours, z `notElem` visited]
    where   neighbours = getNeighbours ms curr

getNeighbours :: MazeSize -> Location -> [Location]
getNeighbours ms curr = foldl (\a (xRel, yRel) -> a ++ inMaze ms (Location (xCurr+xRel) (yCurr+yRel)) ) [] neighboursRelativePosition
    where   xCurr = posX curr
            yCurr = posY curr
            neighboursRelativePosition =    [ (0 , 1)
                                            , (0 ,-1)
                                            , (1 , 0)
                                            , (-1, 0)
                                            ]

inMaze :: MazeSize -> Location -> [Location]
inMaze ms loc 
    | loc `elem` validLocations = [loc]
    | otherwise                 = []
    where validLocations = [ Location x y | x <- [0..((width ms) - 1) ], y <- [0..((height ms) - 1)]]
    

openAPathInMaze :: MazeSize -> Location -> Location -> Maze -> Maze
openAPathInMaze ms next curr m = newMaze
    where   cells = openAPathBetweenCells ms next curr m
            newMaze = insertCellToMaze ms (fst cells) $ insertCellToMaze ms (snd cells) m

insertCellToMaze :: MazeSize -> Cell -> Maze -> Maze
insertCellToMaze ms c m = newMaze
    where   x = posX $ location c
            y = posY $ location c
            index = y * (width ms) + x
            oldCells = cells m
            newMaze = Maze $ (take index oldCells) ++ [c] ++ (drop (index+1) oldCells)

openAPathBetweenCells :: MazeSize -> Location -> Location -> Maze -> (Cell, Cell)
openAPathBetweenCells ms next curr m 
    | xNext > xCurr =   ( Cell {location = curr, wallU = (wallU oldCurrCell), wallR = False              , wallD = (wallD oldCurrCell), wallL = (wallL oldCurrCell) }
                        , Cell {location = next, wallU = (wallU oldNextCell), wallR = (wallU oldNextCell), wallD = (wallD oldNextCell), wallL = False               })
    | xNext < xCurr =   ( Cell {location = curr, wallU = (wallU oldCurrCell), wallR = (wallR oldCurrCell), wallD = (wallD oldCurrCell), wallL = False               }
                        , Cell {location = next, wallU = (wallU oldNextCell), wallR = False              , wallD = (wallD oldNextCell), wallL = (wallL oldNextCell) })
    | yNext > yCurr =   ( Cell {location = curr, wallU = (wallU oldCurrCell), wallR = (wallR oldCurrCell), wallD = False              , wallL = (wallL oldCurrCell) }
                        , Cell {location = next, wallU = False              , wallR = (wallU oldNextCell), wallD = (wallD oldNextCell), wallL = (wallL oldNextCell) })
    | yNext < yCurr =   ( Cell {location = curr, wallU = False              , wallR = (wallR oldCurrCell), wallD = (wallD oldCurrCell), wallL = (wallL oldCurrCell) }
                        , Cell {location = next, wallU = (wallU oldNextCell), wallR = (wallU oldNextCell), wallD = False              , wallL = (wallL oldNextCell) })
    where   xNext = posX next
            xCurr = posX curr
            yNext = posY next
            yCurr = posY curr
            nextIndex = yNext * (width ms) + xNext
            currIndex = yCurr * (width ms) + xCurr
            oldCells = cells m
            oldNextCell = oldCells !! nextIndex
            oldCurrCell = oldCells !! currIndex
