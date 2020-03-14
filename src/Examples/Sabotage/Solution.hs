module Examples.Sabotage.Solution where

import Algebra.Semiring
import Control.Arrow
import Data.List.Split
import DataStructures.Matrix 
import DataStructures.ShortestPath
import Parsing


isThereAnEdge :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> ShortestPath Int (Int,Int)
isThereAnEdge from to mines
  | from `elem` mines = NoPath
  | to `elem` mines   = NoPath
  | otherwise = if ((toY == fromY + 1) || (toY == fromY - 1)) && (toX == fromX)  -- Left or Right move
                then Path 1 [(from,to)]
                else if (toX == fromX + 1) && (toY == fromY) -- Down move 
                then Path 1 [(from,to)]
                else NoPath
  where
    (fromX,fromY) = from 
    (toX,toY) = to 


adjacencyMatrix :: Int -> Int -> [(Int,Int)] -> Matrix (ShortestPath Int (Int,Int))
adjacencyMatrix lengthX lengthY mines = Matrix $ chunksOf (lengthX * lengthY) adjacencyMatrix'
  where 
    adjacencyMatrix' = [ isThereAnEdge from to mines | from <- allCoords, to <- allCoords ]
    allCoords = [(x,y) | x <- [1 .. lengthX], y <- [1 .. lengthY]]


-- TODO: A bit ugly. Maybe use Brent Yorgey's scanner.
interpretInput :: String -> (Int, Int, (Int,Int), (Int,Int), [(Int,Int)])
interpretInput input = (lengthX, lengthY, start, finish, mines)
  where 
    inputs = lines input
    
    (lengthX, lengthY) = (head >>> words >>> map read >>> list2tuple) inputs
    
    nMines = (drop 1 >>> head >>> read) inputs
    mines = (drop 2 >>> take nMines >>> map words >>> map (map read) >>> map list2tuple) inputs 
    
    start = (drop (2 + nMines) >>> head >>> words >>> map read >>> list2tuple) inputs
    finish = (drop (3 + nMines) >>> head >>> words >>> map read >>> list2tuple) inputs


result2string :: ShortestPath Int (Int,Int) -> (Int,Int) -> String
result2string NoPath _ = "0"
result2string (Path n steps) start = unlines $ [ show (n+1)
                                               , show (fst start) <> " " <> show (snd start) ]
                                               <> map (\(_,to) -> show (fst to) <> " " <> show (snd to)) steps


solveOne :: FilePath -> IO ()
solveOne fp= do 
  input <- readFile ("src/Examples/Sabotage/In/sabotage" <> fp <> ".in")
  let (lengthX, lengthY, start, finish, mines) = interpretInput input
  
  let cl = closure $ adjacencyMatrix lengthX lengthY mines
      result = cl ! (tuple2index start lengthY, tuple2index finish lengthY)
  
  writeFile ("src/Examples/Sabotage/Out/sabotage" <> fp <> ".out") (result2string result start)


solve :: IO ()
solve = do 
  let filePaths = map show [1 .. 2]
  mapM_ solveOne filePaths 
  