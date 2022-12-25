data Point = Point { x :: Double
                   , y :: Double }
                   deriving (Show)

type Polygon = [ Point ]                 

vertexMultiplyLR :: Polygon -> Double  
vertexMultiplyLR []         = 0.0
vertexMultiplyLR ( p : [] ) = 0.0
vertexMultiplyLR (      (Point x _)
                 : from@(Point _ y)
                 : ps             ) =
    x * y + vertexMultiplyLR (from : ps)
    
vertexMultiplyRL :: Polygon -> Double  
vertexMultiplyRL []         = 0.0
vertexMultiplyRL ( p : [] ) = 0.0
vertexMultiplyRL (      (Point _ y)
                 : from@(Point x _)
                 : ps             ) =
    x * y + vertexMultiplyRL (from : ps)
    
wrapBack :: [a] -> [a]
wrapBack []       = []
wrapBack xs@(x:_) = xs ++ [x] 

polygonArea :: Polygon -> Double
polygonArea polygon =
    let
        polygon' = wrapBack polygon
        lr       = vertexMultiplyLR polygon'
        rl       = vertexMultiplyRL polygon'
        area     = (lr - rl) / 2
    in
        area
        
-- main :: IO ()
-- main = getContents
--     >>= ( putStrLn
--         . show
--         . polygonArea
--         . map (\[x, y] -> Point x y)
--         . map (map read)
--         . map words
--         . tail
--         . lines )
