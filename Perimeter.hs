data Point = Point { x :: Double
                   , y :: Double }

type Polygon = [ Point ]
        
distance :: Point -> Point -> Double
distance (Point ax ay) (Point bx by) =
    (** 0.5) $ (bx - ax) ** 2 + (by - ay) ** 2 
        
perimeter :: Polygon -> Double
perimeter = sum . map (uncurry distance) . (zip <*> shiftl 1)
    where
        shiftl :: Int -> [a] -> [a]
        shiftl 0 xs = xs
        shiftl _ [] = []
        shiftl n (x:xs) = (shiftl (n - 1) xs) ++ [x]   

-- main :: IO ()
-- main = getContents
--     >>= ( putStrLn 
--         . show
--         . perimeter 
--         . map (\[x, y] -> Point x y)
--         . map (map (read :: String -> Double)) 
--         . map words 
--         . tail 
--         . lines )
