data Tree a = Empty | Branch a (Tree a) (Tree a) 
                deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Q61
countLeaves :: Tree a -> Int 
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- Q61A
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

-- Q62
internals :: Tree a -> [a]
internals Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x l r) = [x] ++ internals l ++ internals r

-- Q62B
atLevel :: Tree a -> Int -> [a]
atLevel _ 0 = error "Indvalid Index"
atLevel Empty _ = []
atLevel (Branch x l r) 1 = [x]
atLevel (Branch x l r) n = atLevel l (n-1) ++ atLevel r (n-1) 

-- Q63
completeBinaryTree :: Int -> Tree Char 
completeBinaryTree = generateTree 1 

generateTree :: Int -> Int -> Tree Char
generateTree x n 
            | x > n = Empty
            | otherwise = Branch 'x' (generateTree (2*x) n) (generateTree (2*x+1) n) 

isCompleteBinaryTree :: Tree Char -> Bool 
isCompleteBinaryTree t = compareTree t $ completeBinaryTree $ countNodes t

compareTree :: Tree a -> Tree a -> Bool
compareTree Empty Empty = True 
compareTree (Branch _ l1 r1) (Branch _ l2 r2) = compareTree l1 l2 && compareTree r1 r2
compareTree _ _ = False 

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = 1 + countNodes l + countNodes r

-- Q64
type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = fst (layoutAux 1 1 t)
  where layoutAux x y Empty = (Empty, x)
        layoutAux x y (Branch a l r) = (Branch (a, (x',y)) l' r', x'')
          where (l', x')  = layoutAux x (y+1) l
                (r', x'') = layoutAux (x'+1) (y+1) r

-- Q67A
treeToString :: Tree Char -> String 
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = [x] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

stringToTree :: String -> Tree Char 
stringToTree "" = Empty
stringToTree [x] = Branch x Empty Empty
stringToTree str = snd (parse str)
  where parse s@(x:xs)      | x == ',' || x == ')' = (s, Empty)
        parse s@(x:y:xs)    | y == ',' || y == ')' = (y:xs, Branch x Empty Empty)
                            | y == '(' = (xs'', Branch x l r) 
                                where   (s'@(m:xs'),l) = parse xs
                                        (s''@(n:xs''),r) = parse xs'                                        
        parse _ = error"invalid tree"

-- Q68
treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder t@(Branch x l r) = [x] ++ treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String 
treeToInorder Empty = ""
treeToInorder (Branch x l r) = treeToInorder l ++ [x] ++ treeToInorder r

preInTree :: String -> String -> Tree Char 
preInTree "" "" = Empty
preInTree [p] [i]   | p == i = Branch p Empty Empty
                    | otherwise = error"invalid order"
preInTree (x:xs) ys = Branch x l r 
    where   l = preInTree pl il
            r = preInTree pr ir 
            (pl,pr) = splitAt (length il) xs
            (il,ir) =  splitAtFirst x ys

-- https://stackoverflow.com/questions/40297001/split-a-list-at-the-first-occurrence-of-an-element
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

-- Q69
ds2tree :: String -> Tree Char 
ds2tree = fst . ds2treeAux
    where   ds2treeAux [] = (Empty,"") 
            ds2treeAux (x:xs) 
                | x == '.' = (Empty,xs)
                | otherwise = (Branch x l r, xs'')
                where   (l,xs') = ds2treeAux xs
                        (r,xs'') = ds2treeAux xs'

tree2ds :: Tree Char -> String 
tree2ds Empty = "."
tree2ds (Branch x l r) = x : tree2ds l ++ tree2ds r