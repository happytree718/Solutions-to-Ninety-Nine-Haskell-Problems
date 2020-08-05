-- Q9 from Q1-10.hs , to be used by Q11
pack :: (Eq a) => [a] -> [[a]]
pack = foldr func []
    where func x [] = [[x]]
          func x ( y : xs )
              | x == head y = (x:y):xs
              | otherwise = [x]:y:xs


-- Q11
data ListType a = Single a | Multiple Int a
  deriving (Show)
encodeModified :: Eq a => [a] -> [ListType a]
encodeModified xs = [y | x <- pack xs, let y = if length x == 1 then Single (head x) else Multiple (length x) (head x)]

-- Q12
decodeModified :: [ListType a] -> [a]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x 

-- Q13
encodeDirect :: Eq a => [a] -> [ListType a]
encodeDirect [] = []
encodeDirect (x:xs) = let (group, rest) = span (==x) xs in 
  checkIfSingle (Multiple (1 + length group) x) : encodeDirect rest
  where checkIfSingle (Multiple n x)
          | n == 1 = (Single x)
          | otherwise = (Multiple n x)

-- Q14
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Q15
repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x

-- Q16
dropEvery :: [a] -> Int -> [a]
dropEvery x n = [ b | (a,b) <- zip [1..] x , (mod a n) /= 0]

-- Q17
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split l@(x:xs) n | n > 0 = (x:ys,zs)
                 | otherwise = ([],l)
                 where (ys,zs) = split xs (n-1)

-- Q18
slice :: [a] -> Int -> Int -> [a]
slice xs x y = [b | (a,b) <- zip [1..] xs, a <= y, a >= x]

-- Q19
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate l@(x:xs) n
  | n > 0 = rotate (xs++[x]) (n - 1)
  | otherwise = rotate l (length l + n)

-- Q20
removeAt :: Int -> [a] -> ([a],[a])
removeAt n xs = ([xs !! (n-1)], map snd $ filter (\(a,b) -> a /= n) $ zip [1..] xs )