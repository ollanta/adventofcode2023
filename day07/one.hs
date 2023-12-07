import Data.Char
import Data.List
import Text.Parsec
import Parsing

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(String, Integer)]
readD = readHand `sepEndBy` newline
  where
    readHand = do
      cards <- many1 (oneOf "AKQJT98765432")
      many1 space
      bid <- number
      return (cards, bid)


solve hands = unlines [
  show sorted
  , show scores
  , show answer
  ]
  where
    answer = sum scores
    sorted = sortOn (scoreHand . fst) hands
    scores = zipWith (*) [1..] . map snd $ sorted

    scoreHand cards = foldl (\acc s -> acc*100 + scoreCard s) typeScore cards
      where
        groups = reverse . sort . map length . groupBy (==) . sort $ cards
        
        countsMatch = and . zipWith (==) groups
        typeScore = fst . head . filter (countsMatch . snd) $ zip [7,6..1] [[5], [4], [3,2], [3], [2,2], [2], []]

        scoreCard c = fst . head . filter ((==c) . snd) $ zip [1..] "23456789TJQKA"
