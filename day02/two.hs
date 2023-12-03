import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Data.List

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Integer, [[(String,Integer)]])]
readD = readCase `sepEndBy` newline
  where
    readCase = do
      string "Game "
      gameID <- number
      string ": "
      rounds <- readRound `sepBy` string "; "
      return (gameID, rounds)

    readRound = readDraw `sepBy` string ", "

    readDraw = do
      n <- number
      space
      color <- word
      return (color, n)

solve :: [(Integer, [[(String,Integer)]])] -> String
solve games = unlines [
  unlines . map show $ minDraws
  , show answer
  ]
  where
    answer = sum . map (product . M.elems) $ minDraws

    minDraws = map findMinDraw games

    findMinDraw (_, rounds) = foldr updateMinDraw M.empty rounds
      where
        updateMinDraw round minDraw = M.unionWith max minDraw (M.fromList round)
