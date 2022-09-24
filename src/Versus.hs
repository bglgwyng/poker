module Versus (Result (..), Versus (..), biggerStronger, strongest, strongestBy) where

data Result = Win | Draw | Lose deriving (Eq, Show)

class Versus a where
  vs :: a -> a -> Result
  vs x y
    | x `win` y = Win
    | x `draw` y = Draw
    | otherwise = Lose

  win :: a -> a -> Bool
  win x y = vs x y == Win
  draw :: a -> a -> Bool
  draw x y = x `vs` y == Draw
  lose :: a -> a -> Bool
  lose = ((Lose ==) .) . vs

biggerStronger :: Ordering -> Result
biggerStronger LT = Lose
biggerStronger EQ = Draw
biggerStronger GT = Win

strongest :: (Foldable f, Versus a) => f a -> a
strongest = strongestBy vs

strongestBy :: Foldable t => (a -> a -> Result) -> t a -> a
strongestBy f =
  foldr1
    ( \x y -> case f x y of
        Win -> x
        _ -> y
    )
