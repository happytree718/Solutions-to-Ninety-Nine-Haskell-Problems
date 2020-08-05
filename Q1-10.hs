-- Q1
myLast :: [a] -> a
myLast [] = error "No end of empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Q2
myButLast :: [a] -> a
myButLast = last . init

-- Q3
elementAt :: [a] -> Int -> a
elementAt arr a
    | a > length arr = error "Index out of bounds"
    | otherwise = arr !! (a - 1)

-- Q4
myLength :: [a] -> Int
myLength = fst . last. zip [1..]

-- Q5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse (xs) ++ [x]

-- Q6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome arr = arr == reverse arr

-- Q7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List(x:xs)) = flatten x ++ flatten (List xs)

-- Q8
compress :: (Eq a) => [a] -> [a]
compress x = foldl (\a b -> if last a == b then a else a++[b]) [head x] x

-- Q9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = let (group,back) = span (head xs==) xs
  in group : pack back  

-- Q10
encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = [(length x, head x) | x <- pack xs]  