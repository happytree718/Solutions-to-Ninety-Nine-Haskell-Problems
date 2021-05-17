-- Q17 from Q11-20.hs to be used by Q21
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split l@(x:xs) n | n > 0 = (x:ys,zs)
                 | otherwise = ([],l)
                 where (ys,zs) = split xs (n-1)

-- Q21
insertAt :: a -> [a] -> Int -> [a]
insertAt xs ys n = let (front,back) = split ys (n-1)
  in front++[xs]++back

-- Q22
range :: Int -> Int -> [Int]
range a b = [x | x <- [1..b] , x >= a]
range' :: Enum a => a -> a -> [a]
range' a b = [a..b]

-- Q26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

-- Q27
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] ->[[[a]]]
group [] _ = [[[]]]
group (n:ns) xs = [ a:b | (a,re) <- combination n xs
                          , b <- group ns re ]
