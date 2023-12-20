import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Data.List

main :: IO ()
main = optimisticInteract readD solve

readD = readLine `sepEndBy` newline
  where
    readLine = do
      name <- many1 (noneOf " ")
      string " -> "
      outs <- word `sepBy` string ", "
      return (name, outs)


solve input = unlines [
  show $ filter (/="broadcaster") . sort $ M.keys inmodules
  , show $ sort . nub . concat . map snd $ M.elems inmodules
  , show $ answer
  ]
  where
    answer = head . drop 1000 $ broadcast inmodules

    inmodules = M.fromList (map niceify input)
      where
        niceify ('%':s, outs) = (s, ('%', outs))
        niceify ('&':s, outs) = (s, ('&', outs))
        niceify (s, outs) = (s, ('b', outs))

    broadcast modules = iterate (\(ff, cn, h, l) -> helper ff cn h (l+1) [(0, "", "broadcaster", modules M.! "broadcaster")]) (flipflop, conjunction, 0, 0)
      where
        flipflop = M.map (const 0) $ M.filter ((=='%') . fst) modules
        conjunction = M.mapWithKey ins $ M.filter ((=='&') . fst) modules
          where
            ins name _ = M.map (const 0) $ M.filter ((name `elem`) . snd) modules 

        helper ff cn h l ((pulse, prev, this, ('%',outs)):ms)
          | pulse == 1 = helper ff cn h l ms
          | otherwise  = helper ff' cn (h+k*pulse') (l+k*(1-pulse')) ms'
          where
            pulse' = 1 - ff M.! this
            ff' = M.insert this pulse' ff
            ms' = ms ++ [(pulse', this, out, modules M.! out) | out <- outs]
            k = length outs
        helper ff cn h l ((pulse, prev, this, ('&',outs)):ms)
          | otherwise = helper ff cn' (h+k*pulse') (l+k*(1-pulse')) ms'
          where
            thiscn = cn M.! this
            thiscn' = M.insert prev pulse thiscn
            cn' = M.insert this thiscn' cn
            pulse' = if all (==1) (M.elems thiscn') then 0 else 1
            ms' = ms ++ [(pulse', this, out, modules M.! out) | out <- outs, M.member out modules]
            k = length outs
        helper ff cn h l ((pulse, prev, this, ('b',outs)):ms)
          | otherwise = helper ff cn (h+k*pulse) (l+k*(1-pulse)) ms'
          where
            ms' = ms ++ [(pulse, this, out, modules M.! out) | out <- outs]
            k = length outs
        helper ff cn h l [] = (ff, cn, h, l)