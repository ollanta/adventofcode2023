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
  showMC $ energized
  , show $ M.size energized
  ]
  where
    inpchart = readM input

    energized = M.fromList $ floodFill ((0,0),'R') contBeam
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
