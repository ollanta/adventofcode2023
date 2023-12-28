import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import qualified Linear.Matrix as X
import Linear.V2

main :: IO ()
main = optimisticInteract readD solve

readD = readLine `sepEndBy` newline
  where
    readLine = do
      n1 <- number
      string ","
      spaces
      n2 <- number
      string ","
      spaces
      n3 <- number
      string " @"
      spaces
      n4 <- mnumber
      string ","
      spaces
      n5 <- mnumber
      string ","
      spaces
      n6 <- mnumber
      return ((n1, n2, n3), (n4, n5, n6))


solve input = unlines [
  show input
  , show $ answer
  ]
  where
    answer = length $ findCrossers (map removeZ input)

    removeZ (a,b) = (removeZ' a, removeZ' b)
      where
        removeZ' :: (Integer, Integer, Integer) -> V2 Double
        removeZ' (x,y,z) = V2 (fromInteger x) (fromInteger y)

    --testarea = (7, 27)
    testarea = (200000000000000, 400000000000000)

    findCrossers inp = filter inarea . filter (/=Nothing) $ crossingPerm
      where
        crossingPerm = [findFutureCrossing a b | (i, a) <- zip [1..] inp, b <- drop i inp]
        inarea (Just (V2 x y)) = helper x && helper y
          where
            helper v = v >= fst testarea && v <= snd testarea

    findFutureCrossing (x1, vx1) (x2, vx2)
      | isInfinite t1 = Nothing
      | isInfinite t2 = Nothing
      | t1 < 0        = Nothing
      | t2 < 0        = Nothing
      | otherwise     = Just (x1 + pure t1 * vx1)
      where
        vmat = V2 vx1 (-vx2)
        V2 t1 t2 =  (x2 - x1) X.*! X.inv22 vmat 
