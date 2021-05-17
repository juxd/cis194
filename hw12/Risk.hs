{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import           Control.Monad.Random
import           Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield
  { attackers :: Army
  , defenders :: Army
  }

-- Exercise 2
battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield attackers defenders) =
  let
    num_attackers = min (attackers - 1) 3
    num_defenders = min (defenders) 2
    roll n
      | n <= 0 = return []
      | otherwise = do
        val  <- die
        rest <- roll (n - 1)
        return (val : rest)
    sortDesc = sortBy (flip compare)
  in
    do
      attacker_rolls <- roll num_attackers
      defender_rolls <- roll num_defenders
      let sorted_attackers = sortDesc attacker_rolls
          sorted_defenders = sortDesc defender_rolls
          (attacker_losses, defender_losses) =
            foldl
                (\acc x -> case (acc, x) of
                  ((atk_losses, def_losses), (atk, def))
                    | atk > def -> (atk_losses, def_losses + 1)
                    | otherwise -> (atk_losses + 1, def_losses)
                )
                (0, 0)
              $ zip sorted_attackers sorted_defenders
      return $ Battlefield (attackers - attacker_losses)
                           (defenders - defender_losses)

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield@(Battlefield attackers defenders) =
  if attackers < 2 || defenders == 0
    then return battlefield
    else battle battlefield >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = repeatM 1000 battlefield 0.0
 where
  repeatM n battlefield acc = if n <= 0
    then return acc
    else do
      (Battlefield _ defenders) <- invade battlefield
      repeatM (n - 1) battlefield (if defenders == 0 then acc + 0.001 else acc)
