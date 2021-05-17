-- | HW4

module Homework4 where

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile ((<) 1) . iterate
  (\x -> if even x then x `div` 2 else x * 3 + 1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf
 where
  insertBalanced a t = case t of
    Leaf -> Node 1 Leaf a Leaf
    Node _ left n right
      | height left <= height right
      -> let new_left = insertBalanced a left
             new_h    = max (height new_left) (height right) + 1
         in  Node new_h new_left n right
      | otherwise
      -> let new_right = insertBalanced a right
             new_h     = max (height left) (height new_right) + 1
         in  Node new_h left n new_right
  height Leaf           = 0
  height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (\x acc a -> acc $ f a x) id xs z

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) $ filter
  (\x -> x `elem` [ i + j + 2 * i * j | i <- [1 .. n], j <- [1 .. n] ])
  [1 .. n]
