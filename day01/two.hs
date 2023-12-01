import Data.Char
import Data.List

main :: IO ()
main = interact run
  where run = show . solve . lines

--solve :: [String] -> Integer
solve l = sum . map (firstlast . toNumbers) $ l
  where
    firstlast :: String -> Integer
    firstlast s = read [head s, last s]

    toNumbers [] = []
    toNumbers s@(c:cs)
      | isNumber c = c : toNumbers cs
      | null matchingNumbers = toNumbers cs
      | otherwise = head matchingNumbers : toNumbers cs
      where
        matchingNumbers = map fst . filter (hasPrefix s . snd) $ numberWords
        hasPrefix s prefix = take (length prefix) s == prefix
        numberWords = zip ['1'..] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
