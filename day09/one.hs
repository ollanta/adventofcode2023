import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[Integer]]
readD = readAll
  where
    readAll = do
      lines <- readLine `sepBy` newline
      return lines

    readLine = do
      numbers <- mnumber `sepBy` char ' '
      return numbers


solve lines = unlines [
  show $ lines
  , show nexts
  , show answer
  ]
  where
    nexts = map extrapolate lines
    answer = sum nexts

    extrapolate numbers
      | all (==0) diffs = last numbers
      | otherwise = last numbers + extrapolate diffs
      where
        diffs = zipWith (-) (tail numbers) numbers