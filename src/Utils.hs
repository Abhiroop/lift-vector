module Utils where

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first, rest) = splitAt n list

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

tuplify4 :: [a] -> Maybe (a,a,a,a)
tuplify4 [a,b,c,d] = Just (a,b,c,d)
tuplify4 _         = Nothing

untuplify4 :: (a,a,a,a) -> Maybe [a]
untuplify4 (a,b,c,d) = Just [a,b,c,d]
untuplify4 _         = Nothing

defaultTuple4 :: Num a => (a,a,a,a)
defaultTuple4 = (0,0,0,0)

defaultList4 :: Num a => [a]
defaultList4 = [0,0,0,0]
