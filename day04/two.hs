import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M

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
  show copies
  , show answer
  ]
  where
    scores = map getScore cards

    getScore (_, winning, own) = length matchingCards
      where
        matchingCards = filter (\n -> any (==n) winning) own

    copies = countCopies scores (repeat 1)
      where
        countCopies [] _ = []
        countCopies (s:ss) (c:cs) = c : countCopies ss cs'
          where
            cs' = zipWith (+) cs (replicate s c ++ repeat 0)

    answer = sum copies