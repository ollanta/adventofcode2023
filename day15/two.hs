import Text.Parsec
import Parsing
import Generic
import Data.List
import Chart2d
import Data.Char
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = readStep `sepBy` char ','
  where
    readStep = do
      many newline
      s <- many1 $ noneOf ",\n"
      many newline
      return s

solve input = unlines [
  show $ input
  , show $ M.toList . M.filter (not . null) $ algo input
  , show $ answer
  ]
  where
    answer = sum . map score $ end
      where
        score (box, l) = (1+box) * sum (zipWith (*) [1..] (map snd l))

    end = M.toList . M.filter (not . null) $ algo input

    algo instrs = helper (M.fromList $ zip [0..255] (repeat [])) instrs
      where
        helper m (i:is) = helper m' is
          where
            (box, op) = getBox 0 i
            label = getLabel i
            m' = case op of
              Just fl ->  eqal m label fl box
              Nothing -> dash m label box
        helper m [] = m

        dash m l box = M.insert box content' m
          where
            content = m M.! box
            content' = filter ((/=l) . fst) content

        eqal m l fl box = M.insert box content' m
          where
            content = m M.! box
            content' = insertLens content

            insertLens ((ol, ofl):cs)
              | l == ol = (l, fl):cs
              | otherwise = (ol, ofl):insertLens cs
            insertLens [] = [(l,fl)]

        getLabel i = takeWhile (not . (`elem` "=-")) i

        getBox :: Int -> String -> (Int, Maybe Int)
        getBox v (c:cs)
          | c == '=' = (v, Just $ read cs)
          | c == '-' = (v, Nothing)
          | otherwise = getBox ((v + ord c) * 17 `rem` 256) cs
