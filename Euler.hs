factorial :: Int -> Int
factorial n = product [1 .. n]

-- e^x
-- Use it like: 10 `e_toThe` 20: 'Ten terms of e^20'

e_toThe :: Int -> Double -> Double
e_toThe terms = sum . map e_toThe' . zip [0..] . take terms . repeat
    where
        e_toThe' :: (Int, Double) -> Double
        e_toThe' ( 0, x) = 1
        e_toThe' ( 1, x) = x
        e_toThe' (at, x) =
            let
                at' = fromIntegral at
                ft' = fromIntegral $ factorial at
            in
                (x ** at') / ft'
