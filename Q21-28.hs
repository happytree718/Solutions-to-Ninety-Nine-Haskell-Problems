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

-- Q23
rnd_select :: [a] -> Int -> IO [a]







-- Q26
--combinations :: Int -> [a] -> [[a]]
