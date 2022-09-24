import Card
import Control.Monad
import Deck
import Versus

main :: IO ()
main = do
  forM_ decks (print . strongestHandWithKickers)
  forM_ decks $ \i ->
    forM decks $ \j ->
      putStrLn $ show (deck i) ++ " vs " ++ show (deck j) ++ " -> " ++ show (i `vs` j)

decks :: [Deck]
decks =
  Deck
    <$> [ Card Diamond <$> [N 2 .. N 9],
          (Card Diamond <$> [A, A, J, Q]) ++ (Card Spade <$> [K, N 2, N 3]),
          Card Diamond <$> [A, A, J, J, J, J, Q],
          Card Diamond <$> A : [N 2 .. N 7],
          Card Diamond <$> [A, K .. N 10]
        ]
