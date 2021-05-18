-- Q31
isqrt :: Int -> Int 
isqrt = floor . sqrt. fromIntegral

isPrime :: Int -> Bool
isPrime k = k > 1 && null [x | x <- [2.. isqrt k], k `mod` x == 0 ]

-- Q32
myGCD :: Int -> Int -> Int 
myGCD a b 
    | b == 0 = abs a
    | otherwise = myGCD b (a `mod` b)   

-- Q33
coprime :: Int -> Int -> Bool 
coprime a b = myGCD a b == 1 

-- Q34
totient :: Int -> Int 
totient a = length [x | x <- [1..a] , coprime a x]

-- Q35
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors k = a++b where 
    a = [head $ filter(\n -> k `mod` n == 0) [2..]]
    b = primeFactors (k `div` head a)        

-- Q36
mult :: [Int] -> [(Int, Int)]
mult [] = []
mult l@(x:xs) = (x, length $ x : takeWhile (==x) xs) : mult (dropWhile (==x) xs)

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult k = mult $ primeFactors k 

-- Q37
totientPhi :: [(Int,Int)] -> Int 
totientPhi [(a,b)] = (a-1)*a^(b-1)
totientPhi (x:xs) = (fst x -1) * fst x ^ (snd x - 1) * totientPhi xs 

-- Q39
primesR :: Int -> Int -> [Int]
primesR a b = [x | x <- [a..b], isPrime x]

-- Q40
goldbach :: Int -> (Int, Int)
goldbach k = head [(a, k-a) | a <- [2..k] , isPrime a, isPrime (k-a)]

-- Q41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = [goldbach n | n <- ev]
    where
        ev = [m | m <- [a..b] , even m] 

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b c = filter(\(x,y) -> x > c && y > c) $ goldbachList a b 