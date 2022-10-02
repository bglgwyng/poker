module Utils (partitions, ensure, when', homo) where

import Control.Applicative
import Control.Arrow
import Control.Monad

partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions (x : xs) = (first (x :) <$> partitions xs) ++ (second (x :) <$> partitions xs)

ensure :: Alternative f => (a -> Bool) -> a -> f a
ensure p x = x <$ guard (p x)

when' :: Alternative f => Bool -> f a -> f a
when' True x = x
when' False _ = empty

homo :: Eq a => [a] -> Maybe a
homo [] = Nothing
homo [x] = Just x
homo (x : xs) = do
  x' <- homo xs
  ensure (x' ==) x