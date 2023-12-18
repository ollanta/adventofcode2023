import Text.Parsec
import Parsing
import Chart2d
import qualified Data.HashMap.Strict as M
import Searches

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Char, Integer, String)]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      c <- oneOf "RLUD"
      spaces
      n <- number
      string " (#"
      s <- many1 alphaNum
      char ')'
      return (c, n, s)

solve input = unlines [
  show $ input
  , showMC $ M.fromList $ zip inpath (repeat '#')
  , showMC $ solution
  , show $ answer
  ]
  where
    solution = full inpath
    answer = M.size solution

    full path = M.fromList $ zip (path ++ innerArea path) (repeat '#')

    innerArea path = floodFill start getNeighs
      where
        (minx, miny) = minimum path
        start = (minx+1, miny+1) -- :D some of the times, it works 100% of the times

        pathM = M.fromList . zip path $ repeat True
        getNeighs c = [c' | c' <- neighbours c, not (c `M.member` pathM)]

    inpath = mapout input

    mapout inps = helper (0,0) inps
      where
        helper c ((d,n,_):is) = newMoves ++ helper c' is
          where
            newMoves = moves c n d
            c' = last newMoves
        helper c _ = []

        moves (x,y) n 'L' = [(x-n',y) | n' <- [1..n]]
        moves (x,y) n 'R' = [(x+n',y) | n' <- [1..n]]
        moves (x,y) n 'U' = [(x,y-n') | n' <- [1..n]]
        moves (x,y) n 'D' = [(x,y+n') | n' <- [1..n]]
