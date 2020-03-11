module DataStructures.Matrix where

import           Algebra.Semiring
import qualified Data.List        as L


data Matrix a = Scalar a
              | Matrix [[a]] 

instance Show a => Show (Matrix a) where
  show (Scalar x) = "Square matrix with diagonal element " <> show x
  show (Matrix rows) = unlines $ map (unwords . map show) rows


type BlockMatrix a = ( Matrix a, Matrix a
                     , Matrix a, Matrix a )


-- mjoin and msplit dangerous
-- What if user passes a Scalar? 

mjoin :: BlockMatrix a -> Matrix a
mjoin ( Matrix a, Matrix b
      , Matrix c, Matrix d ) 
  = Matrix ((a `hcat` b) ++ (c `hcat` d))
  where 
    hcat = zipWith (++)
mjoin _ = error "Cannot join block matrix which contains scalar matrix!"


msplit :: Matrix a -> BlockMatrix a 
msplit (Scalar _) = error "Scalar matrix cannot be splitted into block matrix!"
msplit (Matrix (row:rows)) = ( Matrix [[first]] , Matrix [top]
                             , Matrix left      , Matrix rest )     
  where
    (first:top) = row
    (left,rest) = unzip (map (\(x:xs) -> ([x],xs) )rows)


instance Semiring a => Semiring (Matrix a) where 
  zero = Scalar zero 
  one  = Scalar one 

  Scalar s1 <+> Scalar s2 = Scalar (s1 <+> s2)
  Matrix m1 <+> Matrix m2 = Matrix (zipWith (zipWith (<+>)) m1 m2)
  Scalar s <+> m = m <+> Scalar s
  Matrix [[m]] <+> Scalar s = Matrix [[m <+> s]]
  m <+> s = mjoin ( first <+> s, top
                  , left       , rest <+> s)
    where 
      (first, top, left, rest) = msplit m

  Scalar a <.> Scalar b = Scalar (a <.> b) 
  Scalar a <.> Matrix b = Matrix (map (map (a <.>)) b) 
  Matrix a <.> Scalar b = Matrix (map (map (<.> b)) a) 
  Matrix a <.> Matrix b = let cols = L.transpose b in
                          Matrix [[foldl1 (<+>) (zipWith (<.>) row col) | col <- cols] | row <- a]


instance StarSemiring a => StarSemiring (Matrix a) where 
  closure (Matrix [[x]]) = Matrix [[closure x]] 
  closure m = mjoin ( first' <+> top' <.> rest' <.> left', top' <.> rest'
                    , rest' <.> left'                    , rest')
    where
      (first, top, left, rest) = msplit m 
      first' = closure first 
      top'   = first' <.> top 
      left'  = left <.> first' 
      rest'  = closure (rest <+> left' <.> top)


getElem :: Semiring a => Int -> Int -> Matrix a -> a 
getElem row col (Scalar s) = if row == col then s else zero 
getElem row col (Matrix m) = (m !! row) !! col 


(!) :: Semiring a => Matrix a -> (Int,Int) -> a 
(!) m (row,col) = getElem row col m

pos :: Eq a => a -> Matrix a -> [(Int,Int)]
pos x (Matrix m) = concatMap allOccurancesInRow indexedMatrix 
  where 
    -- Now each row of the matrix is numbered 
    --indexedMatrix :: [([a],Int)]
    indexedMatrix = zip m [1 .. (length m)]

    --allOccurancesInRow :: Eq a => ([a],Int) -> [(Int,Int)]
    allOccurancesInRow (row,rowIdx) = let colIdxs = map (+1) (L.elemIndices x row)
                                      in zip (replicate (length colIdxs) rowIdx) colIdxs


(?) :: Eq a => Matrix a -> a -> [(Int,Int)]
(?) = flip pos