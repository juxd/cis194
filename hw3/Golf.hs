-- | Golf

module Golf where

import           Data.List

-- exercise 1: skips
skips :: [a] -> [[a]]
skips = s 1
 where
  -- `c` is short for "chunk and take" - it chunks the list into chunks of `n`
  -- size, and concats their heads together. Sadly, `chunkOf` is not in the
  -- Haskell STL so this is more efficient than reimplimenting that and mapping
  -- head.
  c _ [] = []
  c n l  = let (h, t) = splitAt n l in head h : c n t
  -- `s` maps c with an `n` that increments as we go down the list
  s _ [] = []
  s n l  = c n l : s (n + 1) (tail l)

-- exercise 2: local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima = l
 where
  l x = case x of
    -- If we have 3 or more elements, we check if second element is maxima then
    -- recurse to tail.
    a : t@(b : c : _) | b > a && b > c -> b : l t
                      | True           -> l t
    -- The remaining case is a list with less than 3 elements, in which case
    -- none can be a maxima.
    _ -> []

-- exercise 3: histogram - old solution
-- histogram :: [Integer] -> String
-- histogram l =
--   -- In each line, we print out '*' (for each count) for every number whose
--   -- count is >= that line's count, of ' ' otherwise.
--   do
--     h <- reverse [1 .. (maximum c)]
--     [ if member i c && c ! i >= h then '*' else ' ' | i <- [0 .. 9] ] ++ "\n"
--   ++ "==========\n0123456789\n"
--  where
--   -- We first generate a count of items from methods imported from Data.Map
--   c = foldr
--     (alter
--       (\x -> case x of
--         Just n -> Just (n + 1)
--         _      -> Just 1
--       )
--     )
--     empty
--     l

-- exercise 3: histogram
histogram :: [Integer] -> String
histogram l =
  -- In each line, we print out '*' (for each count) for every number whose
  -- count is >= that line's count, of ' ' otherwise.
  do
    h <- reverse [1 .. maximum c - 1]
    [ if c !! i > h then '*' else ' ' | i <- [0 .. 9] ] ++ "\n"
  ++ "==========\n0123456789\n"
  -- We first generate a count of items from methods imported from Data.Map
  where c = map length $ group $ sort (l ++ [0 .. 9])
