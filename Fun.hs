module Fun ( tuple
           , first
           , second
           , both
           , shiftl
           , shiftr
           , lscan
           , rscan )
where

tuple :: [a] -> (a, a)
tuple []      = error "Empty List"
tuple [a]     = (a, a)
tuple [a, b]  = (a, b)
tuple (a:b:_) = (a, b)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

second :: (a -> b) -> (c, a) -> (c, b)
second f (a, b) = (a, f b)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

shiftl :: Int -> [a] -> [a]
shiftl 0 xs = xs
shiftl _ [] = []
shiftl n (x:xs) = (shiftl (n - 1) xs) ++ [x]

shiftr :: Int -> [a] -> [a]
shiftr 0 xs = xs
shiftr _ [] = []
shiftr n xs = [last xs] ++ (shiftr (n - 1) $ init xs)

lscan :: (a -> a -> a) -> [a] -> [a]
lscan _ [    ] = []
lscan f (x:xs) = lscan' f x xs
    where
        lscan' :: (b -> a -> b) -> b -> [a] -> [b]
        lscan' _ accumulator [    ] = [accumulator]
        lscan' f accumulator (x:xs) =  accumulator : lscan' f (f accumulator x) xs

rscan :: (a -> a -> a) -> [a] -> [a]
rscan _ [ ]    = [ ]
rscan _ [x]    = [x]
rscan f (x:xs) = let rs@(r:_) = rscan f xs in f x r : rs
