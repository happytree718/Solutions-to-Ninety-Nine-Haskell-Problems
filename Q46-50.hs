-- Q46
and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = not (and' a b) 

nor' :: Bool -> Bool -> Bool
nor' a b = not (or' a b)

xor' :: Eq a => a -> a -> Bool
xor' a b = not (equ' a b) 

impl' :: Bool -> Bool -> Bool
impl' a = or' (not a)

equ' :: Eq a => a -> a -> Bool
equ' a b = a == b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]

-- Q49
gray :: Int -> [String]
gray 0 = [""]
gray n = ['0' : x| x <- gray (n-1) ] ++ ['1' : x| x <- reverse $ gray (n-1) ] 


