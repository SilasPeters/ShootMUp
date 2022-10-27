module SupportiveFunctions where

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) x = min (max l x) u