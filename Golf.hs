-- | Golf

module Golf where

import           Data.Map                       ( (!)
                                                , alter
                                                , empty
                                                , member
                                                )

-- exercise 1: skips
skips :: [a] -> [[a]]
skips = s 1
 where
  -- `c` is short for "chunk and take" - it chunks the list into chunks of `n`
  -- size, and concats their heads together. Sadly, `chunkOf` is not in the
  -- Haskell STL so this is more efficient than using that and mapping head.
  c _ [] = []
  c n l  = let (h, t) = splitAt n l in head h : c n t
  -- `s` maps c with an `n` that increments as we go down the list
  s _ [] = []
  s n l  = c n l : s (n + 1) (tail l)

-- exercise 2: local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima = l
 where
  g a b c = b > a && b > c
  l xs = case xs of
    -- If we have exactly 3 elements in our list, we see if the middle element
    -- is a maxima
    [a, b, c] | g a b c -> [b]
    -- If we have 3 or more, we need to do the same, but recurse to the list
    -- tail.
    a : ls@(b : c : _) | g a b c -> b : l ls
                       | True    -> l ls
    -- The remaining case is a list with less than 3 elements, in which case
    -- none can be a maxima.
    _ -> []

-- exercise 3: histogram
histogram :: [Integer] -> String
histogram l =
  -- From the max counts, we print out '*' (for each count) for every number
  -- whose count is >= that line's count, of ' ' otherwise.
  (   reverse [0 .. (maximum c - 1)]
    >>= (\h ->
          map (\i -> if member i c && c ! i > h then '*' else ' ') [0 .. 9]
            ++ "\n"
        )
    )
    ++ "==========\n0123456789\n"
 where
  -- We first generate a count of items from methods imported from Data.Map
  c = foldr
    (alter
      (\x -> case x of
        Just n -> Just (n + 1)
        _      -> Just 1
      )
    )
    empty
    l
