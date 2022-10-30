module SupportiveFunctions where

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) x = min (max l x) u

roundToDecimals :: Float -> Int -> Float
roundToDecimals t n = fromInteger (floor(t * 10^n)) / (10^n)