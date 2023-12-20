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
  ] ++ unlines (map show $ answer)
  where
    answer = findLoops $ broadcast inmodules

    inmodules = M.fromList (map niceify input)
      where
        niceify ('%':s, outs) = (s, ('%', outs))
        niceify ('&':s, outs) = (s, ('&', outs))
        niceify (s, outs) = (s, ('b', outs))

    findLoops iters = take 10 $ helper (zip [0..] iters)
      where
        helper ((i,(_,_,m)):its)
          | any (\cn -> any (==1) $ M.elems cn) m = (i,m):helper its
          | otherwise = helper its

    broadcast modules = iterate (\(ff, cn, rx) -> helper [] ff cn [(0, "", "broadcaster", modules M.! "broadcaster")]) (flipflop, conjunction, [])
      where
        flipflop = M.map (const 0) $ M.filter ((=='%') . fst) modules
        conjunction = M.mapWithKey ins $ M.filter ((=='&') . fst) modules
          where
            ins name _ = M.map (const 0) $ M.filter ((name `elem`) . snd) modules 

        helper rx ff cn ((pulse, prev, this, ('%',outs)):ms)
          | pulse == 1 = helper rx ff cn ms
          | otherwise  = helper rx ff' cn ms'
          where
            pulse' = 1 - ff M.! this
            ff' = M.insert this pulse' ff
            ms' = ms ++ [(pulse', this, out, modules M.! out) | out <- outs]
            k = length outs
        helper rx ff cn ((pulse, prev, this, ('&',outs)):ms)
          | otherwise = helper rx' ff cn' ms'
          where
            thiscn = cn M.! this
            thiscn' = M.insert prev pulse thiscn
            cn' = M.insert this thiscn' cn
            pulse' = if all (==1) (M.elems thiscn') then 0 else 1
            ms' = ms ++ [(pulse', this, out, modules M.! out) | out <- outs, M.member out modules]
            k = length outs
            rx' = if "rx" `elem` outs then rx ++ [thiscn'] else rx
        helper rx ff cn ((pulse, prev, this, ('b',outs)):ms)
          | otherwise = helper rx ff cn ms'
          where
            ms' = ms ++ [(pulse, this, out, modules M.! out) | out <- outs]
            k = length outs
        helper [] ff cn [] = (ff, cn, [])
        helper rx ff cn [] = (ff, cn, rx)