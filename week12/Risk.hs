{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List 

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

-- 2.
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = 
  fight bf <$> attackerDice bf <*> defenderDice bf

fight :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
fight bf at =
  (\(w, l) -> Battlefield (attackers bf - l) (defenders bf - w))
  . (\(a,d) -> (length a, length d))
  . partition (uncurry (>))
  . zip at 

attackerDice :: Battlefield -> Rand StdGen [DieValue]
attackerDice = capedDice 3 . attackers

defenderDice :: Battlefield -> Rand StdGen [DieValue]
defenderDice = capedDice 2 . defenders

capedDice :: Int -> Army -> Rand StdGen [DieValue]
capedDice cap army = sort <$> randomDice (if army > cap then cap else army) 

randomDice :: Int -> Rand StdGen [DieValue]
randomDice 0 = return []
randomDice n = (:) <$> die <*> randomDice (n-1)

-- 3.

invade :: Battlefield -> Rand StdGen Battlefield
invade bf 
  | defenders bf <= 0 = return bf 
  | attackers bf < 2 = return bf 
  | otherwise = battle bf >>= invade

-- 4.
attackerWinsInvation :: Battlefield -> Rand StdGen Bool 
attackerWinsInvation bf = (== 0) . defenders <$> invade bf 

successProb :: Battlefield -> Rand StdGen Double
successProb bf = 
  (\xs -> (fromIntegral . length $ filter id xs) / fromIntegral (length xs)) <$> replicateM 1000 (attackerWinsInvation bf)

  





