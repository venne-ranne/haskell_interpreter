> module Array where

> import Data.Array

> import While2

Operations provided:
hasE :: a -> Map a b -> Bool
setE :: a -> Map a b -> Map a b
getE :: a -> Map a b -> b
delE :: a -> Map a b -> Map a b

> type Array = [(Int, Val)]
