-- | Homework 7

module JoinList where

import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _  ) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append ((tag l) <> (tag r)) l r

-- Exercise 2
intSize :: (Sized b, Monoid b) => JoinList b a -> Int
intSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0    = Nothing
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ l r) | i < intSize l = indexJ i l
                        | otherwise     = indexJ (i - intSize l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n t =
  let dropJ' n t | n >= intSize t = (intSize t, Empty)
      dropJ' _ Empty              = (0, Empty)
      dropJ' n t@(Single _ _) | n == 0    = (0, t)
                              | otherwise = (1, Empty)
      dropJ' n (Append _ l r) =
        let (dropped_from_l, new_l) = dropJ' n l
            (dropped_from_r, new_r) = dropJ' (n - dropped_from_l) r
        in  (dropped_from_l + dropped_from_r, new_l +++ new_r)
  in  snd (dropJ' n t)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n t =
  let takeJ' n t | n >= intSize t = (intSize t, t)
      takeJ' _ Empty              = (0, Empty)
      takeJ' n t@(Single _ _) | n == 0    = (0, Empty)
                              | otherwise = (1, t)
      takeJ' n (Append _ l r) =
        let (taken_from_l, new_l) = takeJ' n l
            (taken_from_r, new_r) = takeJ' (n - taken_from_l) r
        in  (taken_from_l + taken_from_r, new_l +++ new_r)
  in  snd (takeJ' n t)
