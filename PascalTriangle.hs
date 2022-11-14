links :: [a] -> [[a]]
links [         ] = [    ]
links [    _    ] = [    ]
links (x:r@(y:_)) = [x, y] : links r

-- Exclusive concat.
(+!+) :: Eq a => [a] -> [a] -> [a]
(+!+) xs ys
    | last xs == head ys = init xs ++ ys
    | otherwise          = xs ++ ys

pascalInits :: [Int] -> [Int]
pascalInits xs = [0] ++ xs ++ [0]

pascalConstruct :: [Int] -> [[Int]]
pascalConstruct = links . pascalInits

pascalNext :: [[Int]] -> [[Int]]
pascalNext = pascalConstruct . map sum

pascalDestruct :: [[Int]] -> [Int]
pascalDestruct = foldl1 (+!+)

pascalTriangleOf :: Int -> [[Int]]
pascalTriangleOf = (flip take) ( map     pascalDestruct
                               $ iterate pascalNext
                               $ pascalConstruct [1] )

pascalTriangleOf' :: Int -> [[Int]]
pascalTriangleOf' = map (tail . init) . pascalTriangleOf

-- main :: IO ()
-- main = getLine >>= ( putStrLn
--                    . unlines
--                    . map (unwords . map show)
--                    . pascalTriangleOf'
--                    . read
--                    )
