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

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys -- todo: kan korter
-- Source: https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell