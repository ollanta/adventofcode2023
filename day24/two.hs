{-# LANGUAGE DataKinds #-}
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import qualified Linear.Matrix as X
import qualified Linear.V as V
import Linear.V3
import qualified Data.Vector as Vector
import Data.Maybe

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
  , show $ x0v0
  , show $ fmap round x0v0
  , show $ answer
  ]
  where
    x0v0 = findX0V0 (map toVecs input)

    answer = fmap round $ mat X.!* x0v0
      where
        mat = toV1 [toV6 [1,1,1,0,0,0]]

    toVecs (a,b) = (toVec a, toVec b)
      where
        toVec :: (Integer, Integer, Integer) -> V3 Integer
        toVec (x,y,z) = V3 x y z

    findX0V0 :: [(V3 Integer, V3 Integer)] -> V.V 6 Rational
    findX0V0 vectuples = fmap fromRational $ X.luSolveFinite matr vecr
      where
        (mat, vec) = generateEqSystem vectuples
        matr = fmap (fmap fromInteger) mat
        vecr = fmap fromInteger vec

    {-
    Generate a linear equation system by eliminating collision times
    x0 + vx0*t = xi + vxi*t
    (x0 - xi) = (vxi - vx0)*tt
    (x0 - xi) / (y0 - yi) = (vxi - vx0) / (vyi - vy0)
    vx0*y0 - vy0*x0 = -x0*vyi +y0*xi +vx0*yi -vy0*xi -yi*vxi +vyi*xi
    Since the lhs is the same for any i, we can subtract the rhs for one from another
    and get a linear equation system.
    -}
    generateEqSystem vectuples = (lhs, rhs)
      where
        equations = take 6 [eq | (vi, vj) <- zip vectuples (tail vectuples), eq <- genEquations vi vj]

        rhs = toV6 . map snd $ equations

        lhs = toV6 . map (toV6 . fst) $ equations

    genEquations vti vtj = zip lhs rhs
      where
        ((V3 xi yi zi), (V3 vxi vyi vzi)) = vti 
        ((V3 xj yj zj), (V3 vxj vyj vzj)) = vtj

        lhs = [
          [vyi-vyj, vxj-vxi,       0, yj-yi, xi-xj,     0],
          [      0, vzi-vzj, vyj-vyi,     0, zj-zi, yi-yj]
          ]
        rhs = [
          vyi*xi-vyj*xj + vxj*yj-vxi*yi,
          vzi*yi-vzj*yj + vyj*zj-vyi*zi
          ]

    toV6 :: [a] -> V.V 6 a
    toV6 is = fromJust . V.fromVector . Vector.fromList $ is

    toV1 :: [a] -> V.V 1 a
    toV1 is = fromJust . V.fromVector . Vector.fromList $ is
