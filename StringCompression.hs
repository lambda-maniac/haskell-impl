{-# LANGUAGE LambdaCase #-}

import Data.List (group)

import Control.Arrow (first)

compress :: Eq a => [a] -> [(a, Int)]
compress = map (first head) . (zip <*> map length) . group

compressString :: String -> String
compressString string =
    let
        compression = compress string
        compressed  = concat 
                    $ map (\case (c, 1) -> [c] 
                                 (c, n) -> [c] <> show n)
                    $ compression
    in
        compressed
        
-- main :: IO ()
-- main = getLine >>= putStrLn . compressString
