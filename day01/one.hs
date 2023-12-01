import Data.Char

main :: IO ()
main = interact run
  where run = show . solve . lines

solve :: [String] -> Integer
solve l = sum . map (firstlast . filter isNumber) $ l
  where
    firstlast :: String -> Integer
    firstlast s = read [head s, last s]
