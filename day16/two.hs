import Text.Parsec
import Parsing
import Chart2d
import qualified Data.HashMap.Strict as M
import Searches

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = many1 (oneOf ".|/\\-") `sepEndBy` newline

solve input = unlines [
  show $ input
  , show answer
  ]
  where
    inpchart = readM input

    starts = concat $ [[((0,y),'R'),((mx,y),'L')] | y <- [0..my]] ++ [[((x,0),'D'),((x,my),'U')] | x <- [0..mx]]
      where
        (mx,my) = maximum . M.keys $ inpchart

    answer = maximum . map (M.size . energized) $ starts

    energized start = M.fromList $ floodFill start contBeam
      where
        contBeam (c,d) = filter contained beams
          where
            here = inpchart M.! c
            beams = [(move c d', d') | d' <- splitBeam here d]
            contained (c,_) = M.member c inpchart

        splitBeam '.' d = [d]
        splitBeam '/' 'R' = "U"
        splitBeam '/' 'L' = "D"
        splitBeam '/' 'U' = "R"
        splitBeam '/' 'D' = "L"
        splitBeam '\\' 'R' = "D"
        splitBeam '\\' 'L' = "U"
        splitBeam '\\' 'U' = "L"
        splitBeam '\\' 'D' = "R"
        splitBeam '-' d
          | d `elem` "UD" = "RL"
          | otherwise     = [d]
        splitBeam '|' d
          | d `elem` "RL" = "UD"
          | otherwise     = [d]

        move (x,y) 'R' = (x+1,y)
        move (x,y) 'L' = (x-1,y)
        move (x,y) 'U' = (x,y-1)
        move (x,y) 'D' = (x,y+1)
