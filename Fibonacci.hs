fibonacci :: [Int]
fibonacci = 0 : 1 : [ a + b | (a, b) <- (zip <*> tail) fibonacci ]

fib :: Int -> Int
fib = last . (flip take) fibonacci

-- main :: IO ()
-- main = getLine >>= (putStrLn . show . fib . read)
