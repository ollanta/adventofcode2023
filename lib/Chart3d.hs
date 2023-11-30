module Chart3d where

import qualified Data.HashMap.Strict as M

type Coord = (Integer, Integer, Integer)

getX :: Coord -> Integer
getX (x, _, _) = x

getY :: Coord -> Integer
getY (_, y, _) = y

getZ :: Coord -> Integer
getZ (_, _, z) = z

rotX (x, y, z) = (x, -z, y)
rotY (x, y, z) = (-z, y, x)
rotZ (x, y, z) = (y, -x, z)

-- rotate 0-3 times around the x-axis, then rotate the x-axis to each of the 6 directions
rotations = [rx . rd |
             rx <- take 4 (iterate (rotX.) id),
             rd <- [id, rotY, rotY.rotY, rotY.rotY.rotY, rotZ, rotZ.rotZ.rotZ]]

add (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

sub (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

cmap f (x1, y1, z1) = (f x1, f y1, f z1)

capp (f1, f2, f3) (x2, y2, z2) = (f1 x2, f2 y2, f3 z2)

norm2 (x1, y1, z1) = abs(x1) + abs(y1) + abs(z1)
