module Generic where


takeN n = take (fromInteger . toInteger $ n)

dropN n = drop (fromInteger . toInteger $ n)

lengthN l = fromInteger . toInteger . length $ l