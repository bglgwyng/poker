module Utils (permutation, ensure, when', homo) where

import Control.Applicative
import Control.Arrow
import Control.Monad

permutation :: [a] -> [([a], [a])]
permutation [] = [([], [])]
permutation (x : xs) = (first (x :) <$> permutation xs) ++ (second (x :) <$> permutation xs)

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