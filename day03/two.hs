import Data.Char
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = interact solve

solve inp = unlines [
    show groups,
    show answer
  ]
  where
    chart = readM . lines $ inp

    separateGroups ch = M.foldrWithKey helper (M.empty, []) ch
      where
        helper coord char acc@(visited, groups)
          | not (isNumber char)      = acc
          | coord `M.member` visited = acc
          | otherwise = (visited', newGroup:groups)
          where
            newGroup = findGroup coord visited
            visited' = M.union visited newGroup

        findGroup coord visited = fill [coord] M.empty
          where
            fill [] group = group
            fill coords group = fill coords' group'
              where
                group' = M.union group $ M.fromList [(c, chart M.! c) | c <- coords]
                coords' = [nc | c <- coords,
                                nc <- neighbours8 c,
                                nc `M.member` chart,
                                not (nc `M.member` visited),
                                not (nc `M.member` group'),
                                chart M.! nc /= '.'
                          ]

    (_, groups) = separateGroups $ M.filter isNumber chart

    transformGroup group = (numbers, symbols)
      where
        numbers = findNumbers . showMC . M.filter isNumber $ group
        symbols = filter (not . isNumber) . M.elems $ group

    findNumbers "" = []
    findNumbers s@(c:cs)
      | isNumber c = (read $ takeWhile isNumber s) : findNumbers (dropWhile isNumber s)
      | otherwise  = findNumbers cs

    answer = sum [product numbers
                  | (numbers, symbols) <- map transformGroup groups,
                    length numbers == 2,
                    symbols == "*"]