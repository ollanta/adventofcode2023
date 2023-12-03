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
          | otherwise = (visited', newGroup':groups)
          where
            newGroup = findGroup coord visited
            visited' = M.union visited newGroup

            newGroup' = transformGroup newGroup

        findGroup coord visited = helper [coord] M.empty
          where
            helper [] group = group
            helper coords group = helper coords' group'
              where
                group' = M.union group $ M.fromList [(c, chart M.! c) | c <- coords]
                coords' = [nc | c <- coords,
                                nc <- neighbours8 c,
                                nc `M.member` chart,
                                not (nc `M.member` visited),
                                not (nc `M.member` group'),
                                chart M.! nc /= '.'
                          ]

        transformGroup group
          | length numbers == 2 && symbols == "*" = numbers
          | otherwise = []
          where
            numbers = findNumbers . showMC . M.filter isNumber $ group
            symbols = filter (not . isNumber) . M.elems $ group

    findNumbers "" = []
    findNumbers s@(c:cs)
      | isNumber c = (read $ takeWhile isNumber s) : findNumbers (dropWhile isNumber s)
      | otherwise  = findNumbers cs

    (_, groups) = separateGroups $ M.filter isNumber chart

    answer = sum [product group | group <- groups, not (null group)]
    