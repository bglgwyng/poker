{-# LANGUAGE NamedFieldPuns #-}

module Card (Suit (..), Rank (..), Card (..)) where

import Versus

data Suit = Spade | Diamond | Heart | Clover deriving (Eq, Show)

data Rank = N Int | J | Q | K | A deriving (Eq)

instance Show Rank where
  show (N n) = show n
  show J = "J"
  show Q = "Q"
  show K = "K"
  show A = "A"

data Card = Card {suit :: Suit, rank :: Rank} deriving (Eq)

instance Show Card where
  show Card {suit, rank} =
    ( case suit of
        Spade -> 'S'
        Diamond -> 'D'
        Heart -> 'H'
        Clover -> 'C'
    )
      : show rank

instance Bounded Rank where
  minBound = N 2
  maxBound = A

instance Enum Rank where
  fromEnum (N n) = n
  fromEnum J = 11
  fromEnum Q = 12
  fromEnum K = 13
  fromEnum A = 14

  toEnum x
    | 2 <= x, x <= 10 = N x
  toEnum 11 = J
  toEnum 12 = Q
  toEnum 13 = K
  toEnum 14 = A
  toEnum x = error $ show x ++ " is out of bounds "

  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound
        | fromEnum y >= fromEnum x = maxBound
        | otherwise = minBound

instance Versus Rank where
  x `vs` y =
    case x `compare` y of
      LT -> Lose
      EQ -> Draw
      GT -> Win

instance Ord Rank where
  A <= _ = False
  _ <= A = True
  K <= _ = False
  _ <= K = True
  Q <= _ = False
  _ <= Q = True
  J <= _ = False
  _ <= J = True
  N n1 <= N n2 = n1 <= n2