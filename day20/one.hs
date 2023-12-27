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
  , show $ snd finalState
  , show answer
  ]
  where
    answer = highs * lows
      where
        (_, (highs, lows)) = finalState

    finalState = iterateBroadcast inmodules !! 1000

    inmodules = M.fromList $ map parseModule input
      where
        parseModule ('%':s, outs) = (s, ('%', outs))
        parseModule ('&':s, outs) = (s, ('&', outs))
        parseModule (s, outs)     = (s, ('b', outs))

    iterateBroadcast modules = iterate (\(state, counts) -> broadcastAll state counts initialSignal) initialState
      where
        initialSignal = [(0, "", "broadcaster")]
        initialState = ((flipflop, conjunction), (0, 0))
        flipflop = M.map (const 0) $ M.filter ((=='%') . fst) modules
        conjunction = M.mapWithKey ins $ M.filter ((=='&') . fst) modules
          where
            ins name _ = M.map (const 0) $ M.filter ((name `elem`) . snd) modules

        broadcastAll state counts (signal:signals) = broadcastAll state' counts' (signals ++ moreSignals)
          where
            (state', moreSignals) = broadcast state signal
            counts' = updateCount counts signal
        broadcastAll state counts [] = (state, counts)

        updateCount (highs, lows) (pulse, _, _) = (highs+pulse, lows+1-pulse)

        broadcast state (pulse, prev, from)
          | modules M.!? from == Nothing = (state, [])
          | newPulse == Nothing          = (state, [])
          | otherwise                    = (state', [(pulse', from, out) | out <- outs])
          where
            (mtype, outs) = modules M.! from
            newPulse = runPulse state prev from mtype pulse
            Just (state', pulse') = newPulse

        runPulse state    prev this 'b' pulse = Just (state, pulse)
        runPulse state    prev this '%' 1     = Nothing
        runPulse (ff, cn) prev this '%' 0     = Just ((ff', cn), pulse')
          where
            pulse' = 1 - ff M.! this
            ff' = M.insert this pulse' ff
        runPulse (ff, cn) prev this '&' pulse = Just ((ff, cn'), pulse')
          where
            cn' = M.adjust (M.insert prev pulse) this cn
            pulse' = if all (==1) (M.elems $ cn' M.! this) then 0 else 1
