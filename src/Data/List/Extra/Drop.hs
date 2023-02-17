module Data.List.Extra.Drop where

-- | 'dropWhile' except keep the first of the dropped elements.
--
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs)
  | p x       = x : dropWhile p xs
  | otherwise = x : xs

-- | 'dropWhileEnd' except keep the first of the dropped elements.
--
dropWhileEnd1 :: (a -> Bool) -> [a] -> [a]
dropWhileEnd1 p = go
  where
  -- State where @p@ isn't holding atm.
  go = \case
     []   -> []
     x:xs -> x : if p x then go' [] xs else go xs
  -- State where @p@ is holding atm.
  -- The accumulator holds the elements where @p@ holds (but the first of these).
  go' acc = \case
    []   -> []
    x:xs -> if p x then go' (x:acc) xs else reverse acc ++ x : go xs
