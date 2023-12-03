import Data.Char
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = interact solve

solve inp = unlines [
    showMC stop,
    show answer
  ]
  where
    chart = readM . lines $ inp

    symbols = M.filter (\c -> not (c == '.' || isNumber c)) chart

    fill ch = M.union ch neighbourMap
      where
        neighbourMap = M.fromList [(nc, chart M.! nc) | oc <- M.keys ch,
                                                        nc <- neighbours8 oc,
                                                        M.member nc chart,
                                                        isNumber (chart M.! nc)
                                  ]

    fills = iterate fill symbols
    stop = fst . head . filter (\(a,b) -> a == b) $ zip fills (drop 1 fills)

    answer = sum . findNumbers . showMC $ stop
    
    findNumbers "" = []
    findNumbers s@(c:cs)
      | isNumber c = (read $ takeWhile isNumber s) : findNumbers (dropWhile isNumber s)
      | otherwise  = findNumbers cs
