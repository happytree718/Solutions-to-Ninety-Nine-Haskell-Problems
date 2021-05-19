data Tree a = Empty | Branch a (Tree a) (Tree a) 
                deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Q55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q,r) = (n-1) `quotRem` 2 in
    [Branch 'x' leftTree rightTree | i <- [q..q+r],
                                    leftTree <- cbalTree i,
                                    rightTree <- cbalTree (n-1-i)] 

-- Q56
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ c d) = mirror a d && mirror b c
mirror _ _ = False

symmetric :: Tree a -> Bool 
symmetric Empty = True
symmetric (Branch _ a b) = mirror a b 

-- Q57
add :: Ord a => Tree a -> a -> Tree a
add Empty a = Branch a Empty Empty
add t@(Branch b left right) a = case compare a b of
    LT -> Branch b (add left a) right 
    GT -> Branch b left (add right a)
    EQ -> t

construct :: [Int] -> Tree Int
construct = foldl add Empty 

-- Q58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = [t | t <- cbalTree n, symmetric t]

-- Q59
hbalTree :: a -> Int -> [Tree a]
hbalTree a 0 = [Empty]
hbalTree a 1 = [Branch a Empty Empty]
hbalTree a n = [Branch a left right | (lh,rh) <- [(n-2,n-1),(n-1,n-1),(n-1,n-2)], left <- hbalTree a lh, right <- hbalTree a rh]

-- Q60
minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes 2 = 2
minNodes h = minNodes (h-1) + minNodes (h-2) + 1 

maxHeight :: Int -> Int
maxHeight n = last [h | h <- [0..n] , minNodes h <= n]

maxNodes :: Int -> Int
maxNodes h = 2^h - 1

minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = [t | h <- [minHeight n .. maxHeight n], t <- baltree h n]
    where
        baltree 0 n = [Empty]
        baltree 1 n = [Branch x Empty Empty]
        baltree h n = [Branch x l r | (hl,hr) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)],
            let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
            let max_nl = min (maxNodes hl) (n - 1 - minNodes hr), nl <- [min_nl .. max_nl],
            let nr = n - 1 - nl, l <- baltree hl nl, r <- baltree hr nr]