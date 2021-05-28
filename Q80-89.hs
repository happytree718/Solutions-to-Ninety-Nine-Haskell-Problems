data Graph a = Graph [a] [(a, a)]
                deriving (Show, Eq)

newtype Adjacency a = Adj [(a, [a])]
                deriving (Show, Eq)

newtype Friendly a = Edge [(a, a)]
                deriving (Show, Eq)

graph80 = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
adj80 = Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
fri80 = Edge [('b','c'),('b','f'),('c','f'),('f','k'),('g','h'),('d','d')]

-- Q80
graphToAdj :: Eq a => Graph a -> Adjacency a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph xs ys) = Adj [(vertex, concatMap (findAdj vertex) ys) | vertex <- xs]
    where   findAdj vertex edge@(a,b)
                | a == vertex = [b]
                | b == vertex = [a]
                | otherwise = []

adjToGraph :: Eq a => Adjacency a -> Graph a
adjToGraph (Adj []) = Graph [] []
adjToGraph (Adj ((v, a):vs)) = Graph (v : xs) ((a >>= f) ++ ys)
    where   f x = [(v, x) | not ((v, x) `elem` ys || (x, v) `elem` ys)]
            Graph xs ys = adjToGraph (Adj vs)

graphToFri :: Eq a => Graph a -> Friendly a
graphToFri (Graph [] _ ) = Edge []
graphToFri (Graph xs ys) = Edge (ys ++ [(x,x)|x <- findIsolate xs ys])
    where   findIsolate xs ys = filter (\x -> all (\(a,b) -> x/= a && x/= b) ys) xs

friToGraph :: Eq a => Friendly a -> Graph a
friToGraph (Edge []) = Graph [] []
friToGraph (Edge es) = Graph xs ys
    where   xs = compress $ concatMap (\(a,b)->[a,b]) es
            ys = filter (uncurry (/=)) es
            compress x = foldl (\a b -> if b `elem` a then a else a++[b]) [head x] x

adjToFri :: Eq a => Adjacency a -> Friendly a
adjToFri = graphToFri . adjToGraph

friToAdj :: Eq a => Friendly a -> Adjacency a
friToAdj = graphToAdj . friToGraph

-- Q81 
paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths src dst edge
    | src == dst = [[src]]
    | otherwise = [src:path | e<-edge, fst e == src, path <- paths (snd e) dst (filter (/=e) edge)]

-- Q82
cycles :: Eq a => a -> [(a,a)] -> [[a]]
cycles src edge = [src:path |  e<-edge, fst e == src, path <- paths (snd e) src (filter (/=e) edge)]

