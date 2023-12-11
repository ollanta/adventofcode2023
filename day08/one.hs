import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser (String, [(String, (String, String))])
readD = do
  rl <- word
  many1 newline
  nodes <- readNode `sepEndBy` newline
  return (rl, nodes)
    where
      readNode = do
        node <- word
        string " = ("
        left <- many1 letter
        string ", "
        right <- many1 letter
        string ")"
        return (node, (left, right))


solve (directions, graphAsList) = unlines [
  show $ takeWhile (/="ZZZ") path
  , show answer
  ]
  where
    graph = M.fromList graphAsList
    traverse n d
      | d == 'L' = fst $ graph M.! n
      | d == 'R' = snd $ graph M.! n

    path = scanl traverse "AAA" (cycle directions)
    answer = length $ takeWhile (/="ZZZ") path