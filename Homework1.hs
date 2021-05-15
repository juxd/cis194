{-
stack ghci
:load Homework1.hs
-}

-- exercise 1
toDigits :: Integer -> [Integer]
toDigits a =
  let toDigits' a acc = case divMod a 10 of
        (0, 0) -> acc
        (a, x) -> toDigits' a (x : acc)
  in  toDigits' a []

-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]

data Parity = Odd | Even
doubleEveryOther xs =
  let inner []       = (Even, [])
      inner (x : xs) = case inner xs of
        (Odd , acc) -> (Even, 2 * x : acc)
        (Even, acc) -> (Odd, x : acc)
  in  snd (inner xs)

-- exercise 3
sumDigits :: [Integer] -> Integer

-- use break each number into list of digits them sum those then sum those
sumDigits xs = sum $ map (sum . toDigits) xs

-- exercise 4
validate :: Integer -> Bool

-- lmao
validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits

-- exercise 5 hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1    = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a

main :: IO ()
main = do
  () <- print (doubleEveryOther [1, 2, 3, 4, 5])
  () <- print (sumDigits [69, 420])
  () <- print (validate 121426)
  return ()
