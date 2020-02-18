module Parsing where

list2tuple :: [a] -> (a,a)
list2tuple xs = (xs !! 0, xs !! 1)

tuple2index :: (Int,Int) -> Int -> Int
tuple2index (x,y) lengthY = (x-1)*lengthY + y - 1
