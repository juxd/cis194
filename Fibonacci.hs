{-# LANGUAGE FlexibleInstances #-}
-- | Homework 6

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Exercise 1
fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = let fib' x y = x : fib' y (x + y) in fib' 0 1

-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a tl) = a : streamToList tl

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a tl) = Stream (f a) (streamMap f tl)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed ((+) 1) 0

interleave :: Stream a -> Stream a -> Stream a
interleave (Stream a tl) b = Stream a (interleave b tl)

ruler :: Stream Integer
ruler =
  let ruler' n = interleave (streamRepeat n) (ruler' $ n + 1) in ruler' 0

-- Exercise 6
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger i = Stream i (streamRepeat 0)
  negate (Stream a tl) = Stream (-a) (negate tl)
  (+) (Stream a lt) (Stream b rt) = Stream (a + b) (lt + rt)
  (*) (Stream a lt) r@(Stream b rt) = Stream (a * b) (scaled a rt + lt * r)
    where scaled n (Stream a tl) = Stream (n * a) (scaled n tl)

instance Fractional (Stream Integer) where
  (/) (Stream a lt) (Stream b rt) = s
   where
    scaled n (Stream a tl) = Stream (n * a) (scaled n tl)
    s = Stream (a `div` b) (scaled (1 `div` b) (lt - s * rt))

fibs3 :: Stream Integer
fibs3 = x / Stream 1 (Stream (-1) (Stream (-1) (streamRepeat 0)))

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) = Matrix
    (a00 * b00 + a01 * b10)
    (a00 * b01 + a01 * b11)
    (a10 * b00 + a11 * b10)
    (a10 * b01 + a11 * b11)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n =
  let fibs' 1 = Matrix 1 1 1 0
      fibs' n | even n    = (\m -> m * m) (fibs' (n `div` 2))
              | otherwise = fibs' 1 * fibs' (n - 1)
      (Matrix _ r _ _) = fibs' n
  in  r
