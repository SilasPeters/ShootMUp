module SupportiveFunctions where

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) x = min (max l x) u

roundToDecimals :: Float -> Int -> Float
roundToDecimals t n = fromInteger (floor(t * 10^n)) / (10^n)

-- replaces element at a given index using a given function
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ []     = []
replaceAt i a (x:xs) | i < 0     = error "Cannot replace a value with a negative index"
                     | i == 0    = a : xs
                     | otherwise = x : replaceAt (i - 1) a xs