data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Value Int | App Op Expr Expr

instance Show Expr where
  show (Value x) = show x
  show (App op l r) = "(" ++ show l ++ show op ++ show r ++ ")"

valid :: Op -> Int -> Int -> Bool
valid Div x y = y /= 0 && (x `mod` y == 0)
valid Add x y = x <= y
valid Sub x y = x >= y
valid Mul x y = x <= y
valid _ _ _ = True

-- main function
search :: [Int] -> [Expr]
search lst
  | length lst == 4 = take 1 (searchAll lst)
  | otherwise        = []

searchAll :: [Int] -> [Expr]
searchAll lst = [e | p <- perm lst,
                     (e, v) <- enumerate p,
                     v == 24 ]

enumerate :: [Int] -> [(Expr, Int)]
enumerate [] = []
enumerate [x] = [(Value x, x)]
enumerate lst = [ (App op le re, apply op lv rv) | (l, r) <- split lst,
                                                   (le, lv) <- enumerate l,
                                                   (re, rv) <- enumerate r,
                                                   op <- [Add, Sub, Mul, Div],
                                                   valid op lv rv ]
         
-- perm returns all the permutations of a list, it might contain dup, just like the original list
perm :: [a] -> [[a]]
perm [] = [[]]
perm (x:xs) = [ l | p <- perm xs,
                    l <- interleave x p]

-- interleave an int to an int list, return all possible lists
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : (map (y:) (interleave x ys))

-- split a list into two non-empty lists
split :: [Int] -> [([Int], [Int])]
split [] = []
split [x] = []
split (x:xs) = ([x], xs) : [ (x:l, r) | (l, r) <- split xs ]
