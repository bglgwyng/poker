{-# LANGUAGE FlexibleInstances #-}

module Deck
  ( Deck (..),
    strongestHandWithKickers,
  )
where

import Card
import Control.Arrow
import Data.Function
import Data.List
import Data.Ord
import Hand
import Utils
import Versus

newtype Deck = Deck {deck :: [Card]} deriving (Show)

instance Versus Deck where
  xs `vs` ys =
    let (k1, h1) = strongestHandWithKickers xs
        (k2, h2) = strongestHandWithKickers ys
     in case h1 `vs` h2 of
          Draw -> biggerStronger $ on compare (rank <$>) k1 k2
          x -> x

strongestHandWithKickers :: Deck -> ([Card], Hand)
strongestHandWithKickers =
  strongestBy (on vs snd)
    . ((sortOn (Down . rank) *** strongestHand . sortOn (Down . rank)) <$>)
    . partitions
    . deck