module Examples.KnightsOfNi.Solution where

import Algebra.Numbers
import Algebra.Semiring
import Control.Arrow
import DataStructures.Matrix
import DataStructures.ShortestDistance
import Data.List.Split
import Parsing


data InterpretedInput = InterpretedInput 
                             { nRows :: Int
                             , nCols :: Int 
                             , start :: (Int,Int)
                             , knights :: (Int,Int)
                             , shrubberies :: [(Int,Int)]
                             , impassables :: [(Int,Int)] }

interpretInput :: String -> InterpretedInput
interpretInput input = interpretedInput
  where 
    inputs = lines input 

    (w,h) = (head >>> words >>> map read >>> list2tuple) inputs

    rawMap :: Matrix Int
    rawMap = Matrix $ (drop 1 >>> map words >>> map (map read) >>> concat >>> chunksOf w) inputs 

    interpretedInput = InterpretedInput 
                        { nRows = h
                        , nCols = w
                        , start = head $ rawMap ? 2
                        , knights = head $ rawMap ? 3
                        , shrubberies = rawMap ? 4
                        , impassables = rawMap ? 1 }


isAdjacent :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> ShortestDistance Int
isAdjacent prohibited (fromX,fromY) (toX,toY)
  | (fromX,fromY) `elem` prohibited = Unreachable 
  | (toX,toY) `elem` prohibited = Unreachable
  | ((toX == fromX - 1) || (toX == fromX + 1)) && (fromY == toY) = Distance 1
  | ((toY == fromY - 1) || (toY == fromY + 1)) && (fromX == toX) = Distance 1
  | otherwise = Unreachable 


createAdjacencyMatrix :: ((Int,Int) -> (Int,Int) -> ShortestDistance Int) -> Int -> Int -> Matrix (ShortestDistance Int)
createAdjacencyMatrix creator nRows nCols = Matrix $ chunksOf (nRows * nCols) adjacencyMatrix'
  where 
    adjacencyMatrix' = [ creator from to | from <- allCoords, to <- allCoords ]
    allCoords = [(x,y) | x <- [1 .. nRows], y <- [1 .. nCols]]


startToShrubberiesAdj :: InterpretedInput -> Matrix (ShortestDistance Int)
startToShrubberiesAdj (InterpretedInput r c _ k _ i) = createAdjacencyMatrix (isAdjacent ([k] <> i)) r c

shrubberiesToKnightsAdj :: InterpretedInput -> Matrix (ShortestDistance Int)
shrubberiesToKnightsAdj (InterpretedInput r c _ _ _ i) = createAdjacencyMatrix (isAdjacent i) r c


distsStartToShrubbery :: InterpretedInput -> Matrix (ShortestDistance Int) -> [ShortestDistance Int]
distsStartToShrubbery (InterpretedInput _ c st _ ss _) cl = map (\s -> cl ! (tuple2index st c,tuple2index s c)) ss

distsShrubberiesToKnights :: InterpretedInput -> Matrix (ShortestDistance Int) -> [ShortestDistance Int]
distsShrubberiesToKnights (InterpretedInput _ c _ k ss _) cl = map (\s -> cl ! (tuple2index s c,tuple2index k c)) ss


solveOne :: FilePath -> IO ()
solveOne fp = do 
  input <- readFile ("src/Examples/KnightsOfNi/In/knights" <> fp <> ".in")
  let interpretedInput = interpretInput input
  
  let closureStartToShrubberies = closure $ startToShrubberiesAdj interpretedInput
      closureShrubberiesToKnights = closure $ shrubberiesToKnightsAdj interpretedInput
      
      distsSS = distsStartToShrubbery interpretedInput closureStartToShrubberies
      distsSK = distsShrubberiesToKnights interpretedInput closureShrubberiesToKnights
      dists = zipWith (<.>) distsSS distsSK

      Distance d = minimum dists 

  print d

  writeFile ("src/Examples/KnightsOfNi/Out/knights" <> fp <> ".out") (show d)


solve :: IO ()
solve = do 
  -- Runs out of memory for test cases 7 - 10
  let filePaths = map show [1 .. 6]
  mapM_ solveOne filePaths 
  