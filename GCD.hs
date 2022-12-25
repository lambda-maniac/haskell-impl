factors :: Integer -> [Integer]
factors n = [ m | m <- [ 1 .. n ], n `mod` m == 0 ]

intersection :: Eq a => [a] -> [a] -> [a]
intersection []     _ = []
intersection (x:xs) ys
    | x `elem` ys = x : intersection xs ys
    | otherwise   =     intersection xs ys

-- Prelude has a gcd.
gcd' :: Integer -> Integer -> Integer
gcd' a b =
    let
        aFactors = factors a
        bFactors = factors b
        shared   = intersection aFactors bFactors
        gcd''     = last shared
    in
        gcd''
        
-- main :: IO ()
-- main = getLine
--     >>= ( putStrLn
--         . show
--         . (\[a, b] -> gcd' a b)
--         . map read 
--         . words )
