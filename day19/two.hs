import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Data.List

main :: IO ()
main = optimisticInteract readD solve

data Rule = RComp Char Char Integer String | RSimp String
  deriving (Eq, Show)

readD = do
  workflows <- readWorkflow `sepEndBy` newline
  newline
  ratings <- readRatings `sepEndBy` newline
  return (workflows, ratings)
  where
    readWorkflow = do
      name <- many1 letter
      char '{'
      rules <- readRule `sepBy` char ','
      char '}'
      return (name, rules)

    readRule = choice [try readComp, readSimp]
      where
        readComp = do
          v <- oneOf "xasm"
          c <- oneOf "<>"
          n <- number
          char ':'
          r <- many1 letter
          return $ RComp v c n r
    
        readSimp = do
          r <- many1 letter
          return $ RSimp r

    readRatings = do
      char '{'
      ws <- readRating `sepBy` char ','
      char '}'
      return ws

    readRating = do
      v <- oneOf "xasm"
      char '='
      n <- number
      return (v, n)


solve input = unlines [
  show $ input
  , show $ answer
  ]
  where
    (inworkflows, inratings) = input
    answer = countAcceptable

    countAcceptable = helper ratingsM (wfM M.! "in")
      where
        ratingsM = M.fromList . zip "xasm" $ repeat [1..4000]
        wfM = M.fromList inworkflows

        helper m (RSimp r:_) = cont m r
        helper m (RComp v c n r:ccs) = cont mPositive r + helper mNegative ccs
          where
            (positive, negative) = partition (comp c n) $ m M.! v
            mPositive = M.insert v positive m
            mNegative = M.insert v negative m

        cont m "A" = product . map length . M.elems $ m
        cont m "R" = 0
        cont m r   = helper m (wfM M.! r)

        comp '<' n v = v < n
        comp '>' n v = v > n



