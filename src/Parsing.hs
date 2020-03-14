module Parsing where

list2tuple :: [a] -> (a,a)
list2tuple [x1,x2] = (x1, x2)

tuple2index :: (Int,Int) -> Int -> Int
tuple2index (x,y) lengthY = x*lengthY + y
