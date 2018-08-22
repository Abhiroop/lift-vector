module Utils where

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first, rest) = splitAt n list

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

tuplify4 :: [a] -> Maybe (a, a, a, a)
tuplify4 [a, b, c, d] = Just (a, b, c, d)
tuplify4 _ = Nothing

untuplify4 :: (a, a, a, a) -> [a]
untuplify4 (a, b, c, d) = [a, b, c, d]

defaultTuple4 :: Num a => (a, a, a, a)
defaultTuple4 = (0, 0, 0, 0)

tuplify2 :: [a] -> Maybe (a, a)
tuplify2 [a, b] = Just (a, b)
tuplify2 _ = Nothing

untuplify2 :: (a, a) -> [a]
untuplify2 (a, b) = [a, b]

defaultTuple2 :: Num a => (a, a)
defaultTuple2 = (0, 0)
