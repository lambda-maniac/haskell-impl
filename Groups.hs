import Prelude hiding (takeWhile, dropWhile)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile predicate (x:xs)
    | predicate x = x : takeWhile predicate xs
    | otherwise   = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile predicate (x:xs)
    | predicate x = dropWhile predicate xs
    | otherwise   = x : xs

separate :: (a -> Bool) -> [a] -> ([a], [a])
separate predicate xs = separateWhile' predicate xs ([], [])
    where
        separateWhile' :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
        separateWhile' _ [] result = result
        separateWhile'
            predicate list result@(parsed, _)
            | not $ null parsed' = separateWhile' predicate rest' reduced
            | otherwise          = result
            where
                (parsed', rest') = separate' predicate list
                reduced          = ( parsed ++ parsed', rest' )

                separate' :: (a -> Bool) -> [a] -> ([a], [a])
                separate' _ [] = ([], [])
                separate' predicate ls@(x:xs)
                    | predicate x = ([x], xs)
                    | otherwise   = ([ ], ls)

groupA :: Eq a => [a] -> [[a]]
groupA []       = []
groupA ls@(x:_) =
    let
        (taken, rest) = separate (== x) ls
    in
        taken : groupA rest

groupB :: Eq a => [a] -> [[a]]
groupB []       = [   ]
groupB ls@(x:_) = taken : groupB dropped
    where taken   = takeWhile (== x) ls
          dropped = dropWhile (== x) ls
