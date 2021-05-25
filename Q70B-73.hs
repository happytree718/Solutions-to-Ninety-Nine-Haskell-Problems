data Tree a = Node a [Tree a]
        deriving (Eq, Show)

-- Q70C
nnodes :: Tree a -> Int
nnodes (Node _ xs) = 1 + sum (map nnodes xs)

-- Q70
stringToTree :: String -> Tree Char
stringToTree (x:xs) = Node x (fst(stringToTreeAux xs))
        where   stringToTreeAux (y:ys)
                        | y == '^' = ([],ys)
                        | otherwise = (Node y t : t',ts')
                                where   (t,ts) = stringToTreeAux ys
                                        (t',ts') = stringToTreeAux ts

treeToString :: Tree Char -> String
treeToString (Node x xs) = x : concatMap treeToString xs ++ ['^']

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

-- Q71
ipl :: Tree a -> Int
ipl = internalpath 1
        where   internalpath n (Node x xs) = length xs * n + sum (map (internalpath (n+1)) xs)

-- Q72
bottomUp :: Tree Char -> String
bottomUp (Node x xs) = concatMap bottomUp xs ++ [x]

-- Q73
lisp :: Tree Char -> String
lisp (Node x []) = [x]
lisp (Node x xs) = '(':x: concatMap lisp xs ++ [')']