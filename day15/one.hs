import Text.Parsec
import Parsing
import Data.Char

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = readStep `sepBy` char ','
  where
    readStep = do
      many newline
      s <- many1 $ noneOf ",\n"
      return s

solve input = unlines [
  show $ input
  , show $ map hash input
  , show $ answer
  ]
  where
    answer = sum $ map hash input

    hash str = hash' 0 str
      where
        hash' v (c:cs) = hash' ((v + ord c) * 17 `rem` 256) cs
        hash' v [] = v