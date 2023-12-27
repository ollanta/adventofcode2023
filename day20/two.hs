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
  , show answer
  ] ++ unlines (map show $ findLoops 3 $ iterateBroadcast inmodules)
  where
    answer = foldr1 lcm . map (fromInteger . head . snd) . findLoops 1 $ iterateBroadcast inmodules

    inmodules = M.fromList (map niceify input)
      where
        niceify ('%':s, outs) = (s, ('%', outs))
        niceify ('&':s, outs) = (s, ('&', outs))
        niceify (s, outs) = (s, ('b', outs))

    -- Making a lot of assumptions about independent nice cycles
    findLoops n iters = [(activator, take n [i | (i,a) <- activations, a == activator]) | activator <- activatorActivators]
      where
        activatorActivators = M.keys . head $ concatMap snd iters

        activations = [(i, activator) | (i, activations) <- zip [0..] (map nicer iters), activator <- activations]

        nicer (_, []) = []
        nicer (_, rxacts) = nub . map fst . filter ((==1) . snd) . concatMap M.toList $ rxacts


    iterateBroadcast modules = iterate (\(state, rxacts) -> broadcastAll state [] initialSignal) initialState
      where
        initialSignal = [(0, "", "broadcaster")]
        initialState = ((flipflop, conjunction), [])
        flipflop = M.map (const 0) $ M.filter ((=='%') . fst) modules
        conjunction = M.mapWithKey ins $ M.filter ((=='&') . fst) modules
          where
            ins name _ = M.map (const 0) $ M.filter ((name `elem`) . snd) modules
        rxActivators = M.keys $ M.filter (elem "rx" . snd) modules

        broadcastAll state rxacts (signal:signals) = broadcastAll state' (rxacts++rxacts') (signals ++ moreSignals)
          where
            (state', rxacts', moreSignals) = broadcast state signal
        broadcastAll state rxacts [] = (state, rxacts)

        broadcast state (pulse, prev, from)
          | modules M.!? from == Nothing = (state, [], [])
          | newPulse == Nothing          = (state, [], [])
          | otherwise                    = (state', rxacts, [(pulse', from, out) | out <- outs])
          where
            (mtype, outs) = modules M.! from
            newPulse = runPulse state prev from mtype pulse
            Just (state', rxacts, pulse') = newPulse

        runPulse state    prev this 'b' pulse = Just (state, [], pulse)
        runPulse state    prev this '%' 1     = Nothing
        runPulse (ff, cn) prev this '%' 0     = Just ((ff', cn), [], pulse')
          where
            pulse' = 1 - ff M.! this
            ff' = M.insert this pulse' ff
        runPulse (ff, cn) prev this '&' pulse = Just ((ff, cn'), rxacts, pulse')
          where
            cn' = M.adjust (M.insert prev pulse) this cn
            pulse' = if all (==1) (M.elems $ cn' M.! this) then 0 else 1
            rxacts = if this `elem` rxActivators then [cn' M.! this] else []
