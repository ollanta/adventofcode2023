module Chart2d where

import qualified Data.HashMap.Strict as M

type Coord = (Integer, Integer)

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

neighbours8 :: Coord -> [Coord]
neighbours8 (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x'/=x || y'/=y]

getX :: Coord -> Integer
getX (x, _) = x

getY :: Coord -> Integer
getY (_, y) = y


readM :: [[a]] -> M.HashMap Coord a
readM rows = M.fromList chartelems
  where
    chartelems = [((x, y), v) | (y, row) <- zip [0..] rows,
                                (x, v)   <- zip [0..] row]

showMC :: M.HashMap Coord Char -> String
showMC chart = showM chart (:[])

showMS :: Show a => M.HashMap Coord a -> String
showMS chart = showM chart show

showM :: M.HashMap Coord a -> (a -> String) -> String
showM chart shower = unlines rows
  where
    minx = minimum . map getX . M.keys $ chart
    maxx = maximum . map getX . M.keys $ chart

    miny = minimum . map getY . M.keys $ chart
    maxy = maximum . map getY . M.keys $ chart

    maybeshower (Just v) = shower v
    maybeshower Nothing  = " "

    rows = [concat [maybeshower (M.lookup (x, y) chart) | x <- [minx..maxx]] | y <- [miny..maxy]]

liftf2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

add c1 c2 = liftf2 (+) c1 c2

sub c1 c2 = liftf2 (-) c1 c2
