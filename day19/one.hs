import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M

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
    answer = score . filter accepted $ inratings
      where
        score ratings = sum [v | rating <- ratings, (k, v) <- rating]

    accepted ratings = helper (wfM M.! "in")
      where
        ratingsM = M.fromList ratings
        wfM = M.fromList inworkflows

        helper (RSimp r:_) = cont r
        helper (RComp v c n r:ccs)
          | comp v c n = cont r
          | otherwise    = helper ccs

        cont "A" = True
        cont "R" = False
        cont r   = helper (wfM M.! r)

        comp v '<' n = (ratingsM M.! v) < n
        comp v '>' n = (ratingsM M.! v) > n



