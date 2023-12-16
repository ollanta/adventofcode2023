import Text.Parsec
import Parsing
import Data.Char
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(String, Maybe Int)]
readD = readStep `sepBy` char ','
  where
    readStep = do
      many newline
      s <- many1 alphaNum
      c <- choice [readEq, readMin]
      return (s, c)

    readEq = (Just . fromInteger) <$> (char '=' >> number)
    readMin = char '-' >> pure Nothing

solve input = unlines [
  show $ input
  , show $ answer
  ]
  where
    answer = sum . map score . M.toList . M.map scoreContent $ algo input
      where
        scoreContent c = sum . zipWith (*) [1..] $ map snd c
        score (box, contentScore) = (1+box) * contentScore

    hash str = foldl updateHash 0 str
      where
        updateHash v c = (v + ord c) * 17 `rem` 256

    algo instrs = helper startState instrs
      where
        startState = (M.fromList $ zip [0..255] (repeat []))

        helper m ((label,op):is) = helper m' is
          where
            box = hash label
            m' = M.adjust (updateBox label op) box m
        helper m [] = m

        updateBox l (Just fl) content = insertLens content
          where
            insertLens ((ol, ofl):cs)
              | l == ol   = (l, fl):cs
              | otherwise = (ol, ofl):insertLens cs
            insertLens [] = [(l,fl)]
        updateBox l Nothing content = filter ((/=l) . fst) content
