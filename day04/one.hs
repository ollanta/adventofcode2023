import Data.Char
import Text.Parsec
import Parsing

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Integer, [Integer], [Integer])]
readD = readCase `sepEndBy` newline
  where
    readCase = do
      cardID <- readCardID
      nums1 <- readNumbers
      string "|"
      nums2 <- readNumbers
      return (cardID, nums1, nums2)

    readCardID = do
      string "Card"
      many1 space
      cardID <- number
      string ":"
      return cardID

    readNumbers = do
      many1 space
      nums <- number `sepEndBy` many1 (char ' ')
      return nums



solve cards = unlines [
  unlines . map show $ cards
  , show answer
  ]
  where
    scores = map getScore cards

    getScore (_, winning, own) = scoreHelper matchingCards
      where
        matchingCards = filter (\n -> any (==n) winning) own

        scoreHelper [] = 0
        scoreHelper l  = 2 ^ (length l - 1)

    answer = sum scores
