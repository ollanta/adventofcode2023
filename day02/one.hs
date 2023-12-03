import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M

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
  unlines . map show $ goodGames
  , show answer
  ]
  where
    answer = sum . map fst $ goodGames
    goodGames = filter isGoodGame games

    isGoodGame (_, rounds) = and . map isGoodRound $ rounds
      where
        isGoodRound = and . map isGoodDraw

        isGoodDraw ("red", x) = x <= 12
        isGoodDraw ("green", x) = x <= 13
        isGoodDraw ("blue", x) = x <= 14
        isGoodDraw _ = True
        