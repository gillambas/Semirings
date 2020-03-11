module Examples.PonyExpress.Solution where

import Algebra.Semiring
import Control.Arrow
import qualified Data.IntMap as IntMap
import DataStructures.Matrix 
import DataStructures.ShortestDistance
import Parsing


-----------------------------------------------------------------------------------
-----------------------         SOLVE TEST CASES            -----------------------
-----------------------------------------------------------------------------------
solve :: IO ()
solve = mapM_ solveInput ["Small", "Large"]

solveInput :: String -> IO ()
solveInput dataSize = do
  input <- readFile (mconcat ["src/Examples/PonyExpress/In/", dataSize, ".in"])
  let input' = lines input 
      nCases = (head >>> read) input'
      tcs = createTestCases (drop 1 input')
      solns = map solveTestCase tcs
      output = createOutput nCases solns 
  writeFile (mconcat ["src/Examples/PonyExpress/Out/", dataSize, ".out"]) output

solveTestCase :: TestCase -> [Double] 
solveTestCase tc = solns
  where 
      shortestDists = shortestDistances (cityDists tc)
      times = dists2times tc shortestDists
      shortestTimes = closure times
      solns = map (\(x,y) -> fromDistance $ shortestTimes ! (x-1,y-1)) (deliveries tc)
-----------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
---------------             ADJECENCY MATRIX OPERATIONS             ---------------
-----------------------------------------------------------------------------------
shortestDistances :: Matrix Int -> Matrix ShortestDistance
shortestDistances (Matrix m) = closure $ Matrix (map (map (\d -> if d == -1 then Unreachable else Distance (fromIntegral d))) m)

dists2times :: TestCase -> Matrix ShortestDistance -> Matrix ShortestDistance 
dists2times tc (Matrix  m) = Matrix $ zipWith (changeRows tc) [1 .. (nCities tc)] m

changeRows :: TestCase -> Int -> [ShortestDistance] -> [ShortestDistance]
changeRows (TestCase _ s h _ _) city = map go
  where 
    go :: ShortestDistance -> ShortestDistance 
    go Unreachable  = Unreachable 
    go (Distance d) = if fromIntegral (h IntMap.! city) >= d then Distance (d/fromIntegral (s IntMap.! city)) else Unreachable
-----------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
-----------------------               PARSING               -----------------------
-----------------------------------------------------------------------------------
type City     = Int 

type SpeedMap     = IntMap.IntMap Int  -- Speed horse in each city attains
type HorseDistMap = IntMap.IntMap Int  -- Distance horse of each city can travel

data TestCase = TestCase 
                  { nCities         :: Int 
                  , speeds          :: SpeedMap 
                  , horseDists      :: HorseDistMap  
                  , cityDists       :: Matrix Int
                  , deliveries      :: [(City,City)] }
                deriving (Show)


parseTestCase :: [String] -> (TestCase,[String])
parseTestCase s = (TestCase n speeds horseDists cityDists deliveries, remStr)
  where 
    (n,q) = (head >>> words >>> map read >>> list2tuple) s
    (ds,ss) = (tail >>> take n >>> map (list2tuple . map read . words) >>> unzip) s
    
    horseDists = IntMap.fromList $ zip [1 .. n] ds 
    speeds = IntMap.fromList $ zip [1 .. n] ss 

    cs = (tail >>> drop n >>> take n >>> map (map read . words)) s
    cityDists = Matrix cs 

    deliveries = (drop (1+2*n) >>> take q >>> map (list2tuple . map read . words)) s

    remStr = drop (1 + 2*n + q) s  


createTestCases :: [String] -> [TestCase]
createTestCases [] = [] 
createTestCases s = let (tc,s') = parseTestCase s
                    in  tc : createTestCases s'
-----------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
-----------------------          DISPLAY OUTPUT             -----------------------
-----------------------------------------------------------------------------------
list2str :: Show a => [a] -> String
list2str = unwords . map show

createOutput :: Int -> [[Double]] -> String 
createOutput nCases times = unlines $ zipWith (\n t -> mconcat ["Case #", show n, ": ", list2str t]) [1 .. nCases] times 
-----------------------------------------------------------------------------------
