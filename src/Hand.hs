{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Hand (Hand (..), strongestHand) where

import Card
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Utils
import Versus

data Hand
  = NoPairs
  | OnePair Rank
  | TwoPairs (Rank, Rank)
  | Trips Rank
  | Straight Rank
  | Flush [Rank]
  | FullHouse (Rank, Rank)
  | FourCards Rank
  | StraightFlush Rank
  deriving (Eq, Show)

instance Versus Hand where
  StraightFlush r1 `vs` StraightFlush r2 = r1 `vs` r2
  StraightFlush {} `vs` _ = Win
  _ `vs` StraightFlush {} = Lose
  FourCards r1 `vs` FourCards r2 = r1 `vs` r2
  FourCards {} `vs` _ = Win
  _ `vs` FourCards {} = Lose
  FullHouse (r1, r2) `vs` FullHouse (r1', r2')
    | r1 `draw` r1' = r2 `vs` r2'
    | otherwise = r1 `vs` r1'
  FullHouse {} `vs` _ = Win
  _ `vs` FullHouse {} = Lose
  Flush r1 `vs` Flush r2 = biggerStronger $ r1 `compare` r2
  Flush {} `vs` _ = Win
  _ `vs` Flush {} = Lose
  Straight r1 `vs` Straight r2 = r1 `vs` r2
  Straight {} `vs` _ = Win
  _ `vs` Straight {} = Lose
  Trips r1 `vs` Trips r2 = r1 `vs` r2
  Trips {} `vs` _ = Win
  _ `vs` Trips {} = Lose
  TwoPairs r1 `vs` TwoPairs r2 = biggerStronger $ r1 `compare` r2
  TwoPairs {} `vs` _ = Win
  _ `vs` TwoPairs {} = Lose
  OnePair r1 `vs` OnePair r2 = vs r1 r2
  OnePair {} `vs` _ = Win
  _ `vs` OnePair {} = Lose
  NoPairs `vs` NoPairs = Draw

-- foo :: Int -> String
-- foo = undefined

-- bar :: Int -> Bool
-- bar = undefined

-- baz = foo <* bar
strongestHand :: [Card] -> Hand
strongestHand xs =
  asum
    ( [ (StraightFlush <$>) . uncurry (<*) . (straight &&& flush),
        (FourCards <$>) . fourCard,
        (FullHouse <$>) . fullHouse,
        (Flush <$>) . flush,
        (Straight <$>) . straight,
        (Trips <$>) . trips,
        (TwoPairs <$>) . twoPair,
        (OnePair <$>) . onePair
      ]
        <&> ($ xs)
    )
    & fromMaybe NoPairs

onePair :: [Card] -> Maybe Rank
onePair [Card {rank = r1}, Card {rank = r2}]
  | r1 == r2 = Just r1
onePair _ = Nothing

twoPair :: [Card] -> Maybe (Rank, Rank)
twoPair xs
  | [r1, r2, r3, r4] <- rank <$> xs,
    r1 == r2,
    r3 == r4 =
      pure (r1, r3)
twoPair _ = Nothing

trips :: [Card] -> Maybe Rank
trips xs
  | length xs == 3,
    Just r <- homo $ rank <$> xs =
      pure r
trips _ = Nothing

fullHouse :: [Card] -> Maybe (Rank, Rank)
fullHouse xs
  | [r1, r2, r3, r4, r5] <- rank <$> xs =
      if
          | r1 == r2,
            Just r <- homo [r3, r4, r5] ->
              pure (r, r1)
          | Just r <- homo [r1, r2, r3],
            r4 == r5 ->
              pure (r, r4)
          | otherwise -> Nothing
fullHouse _ = Nothing

fourCard :: [Card] -> Maybe Rank
fourCard xs = foo xs
  where
    foo =
      ((== 4) . length &&& (homo . (rank <$>)))
        >>> uncurry when'

flush :: [Card] -> Maybe [Rank]
flush xs = do
  homo $ suit <$> xs
  guard $ length xs == 5
  pure $ rank <$> xs

straight :: [Card] -> Maybe Rank
straight xs
  | length xs == 5,
    ranks@(r : _) <- rank <$> xs =
      if
          | r /= minBound, take 5 [r, pred r ..] == ranks -> pure r
          | A : [N 5, N 4 ..] == ranks -> pure $ N 5
          | otherwise -> Nothing
straight _ = Nothing
