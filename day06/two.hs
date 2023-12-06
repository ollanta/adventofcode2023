import Data.Char
import Text.Parsec
import Parsing

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser ([Integer], [Integer])
readD = readAll
  where
    readAll = do
      string "Time:"
      many1 space
      times <- number `sepEndBy` many space
      string "Distance:"
      many1 space
      distances <- number `sepEndBy` many space
      return (times, distances)


solve (times, distances) = unlines [
  show times
  , show distances
  , show answer
  ]
  where
    betterDistances time record = filter (>record) [t*(time-t) | t <- [1..time-1]]

    merge = read . foldr ((++) . show) ""
    answer = length $ betterDistances (merge times) (merge distances)
