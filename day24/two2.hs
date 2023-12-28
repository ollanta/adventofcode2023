import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import qualified Linear.Matrix as X
import Linear.V3

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

-- Search hoping the first collision is early, but it's at t=58943605754

solve input = unlines [
  show input
  , show $ "eyyy"
  ] ++ unlines (map show answer)
  where
    answer = take 100 $ findPath (map toVecs input)

    toVecs (a,b) = (toVec a, toVec b)
      where
        toVec :: (Integer, Integer, Integer) -> V3 Integer
        toVec (x,y,z) = V3 x y z

    findPath inp = [(s, inferX0V0 (a, t1) (b, t2)) | s@((a, t1), (b, t2)) <- starts, crossesAll (a, t1) (b, t2)]
      where
        inpM :: M.HashMap Integer (V3 Integer, V3 Integer)
        inpM = M.fromList . zip [0..] $ inp
        starts = [((a, t1),(b, t2)) | t1 <- [1..10], t2 <- [t1+1..t1+10], a <- M.keys inpM, b <- M.keys inpM, a/=b]

        crossesAll (i1, t1) (i2, t2) = case inferredX0V0 of
            Nothing -> False
            Just (x0, v0) -> all (crossesAfterT t2 (x0, v0)) otherS
          where
            inferredX0V0 = inferX0V0 (i1, t1) (i2, t2)
            otherS = [tup | (i, tup) <- M.toList inpM, i /= i1, i /= i2]

        inferX0V0 (i1, t1) (i2, t2)
          | vfrac /= fmap fromInteger ivfrac = Nothing -- noninteger velocity
          | otherwise = Just (x0, ivfrac)
          where
            (x1, v1) = inpM M.! i1
            (x2, v2) = inpM M.! i2

            x1t1 = x1 + pure t1 * v1
            x2t2 = x2 + pure t2 * v2

            vfrac  = fmap fromInteger (x2t2 - x1t1) / fromInteger (t2-t1)
            ivfrac = fmap round vfrac

            x0 = x1t1 - pure t1 * ivfrac

    crossesAfterT tmin (ix0, iv0) (ix, iv)
      | t < fromInteger tmin = False
      | otherwise            = (ix0 - ix) == pure t * (iv - iv0)
      where
        (V3 tx ty tz) = fmap fromInteger (ix0 - ix) / fmap fromInteger (iv - iv0)
        t = round . head $ filter (not . isNaN) [tx, ty, tz]
