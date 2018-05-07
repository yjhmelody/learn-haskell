factorial :: Integer -> Integer
factorial n = if n < 0 then error "n is less than 0"
        else if n == 0 then 1
        else n * factorial (n-1)
    

mygcd :: Int -> Int -> Int
mygcd x y = if y == 0 then x else mygcd y (mod x y)

power :: Int -> Int -> Int
power _ 0 = 1
power x n
    | odd n = let p = power x ((n-1) `div` 2) in x * p * p
    | otherwise = let p = power x (n `div` 2) in p * p


-- 在之前的章节中，介绍了很多库函数。现在，在本节里自己动手定义它们。

product' [] = 1
product'(x:xs) = x * product' xs

cons :: a -> [a] -> [a]
cons = (:)

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

take' :: Int -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

-- 5.2.1
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x ys = foldr (\xx -> \acc -> if x /= xx then xx:acc else acc) [] ys

-- 5.2.2
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) = if n <= 0 then x:xs else drop' (n-1) xs

-- stackoverflow
-- total [] = 0
-- total (x:xs) = x + total xs

-- tailrec
-- total' [] n = n
-- total' (x:xs) n = total' xs (n+x)
-- total xs = total' xs 0
-- 表面上，这样的优化本来可以使 Haskell 不需要使用更多的空间，可是Haskell 是一个默认设置为惰性求值的语言，还是会产生问题。
-- 由于惰性求值在使用尾递归时也可能会产生和扩展递归一样的问题，因此，在 total'
-- 函数调用到递归基本条件前，参数 n 只参与和 x 的加法运算，而并不作为结果使用，即 n
-- 的具体值在递归到达基本条件前不需要被计算。因此，Haskell 还是会把这些值暂时存于内存
-- 中，等到需要的时候才计算。

-- total [1,2,3] 0
-- = total' [2,3] (1 + 0)
-- = total' [3] (2 + (1 + 0))
-- = total' [] (3 + (2 + (1 + 0)))
-- = (3 + (2 + (1 + 0)))
-- = (3 + (2 + 1))
-- = (3 + 3)
-- = 6

-- 这样，需要使用叹号模式（bang pattern）匹配或者 ($!) 运算符来强制 Haskell 对 total' 的第二个参数进行求值。
-- tailrec
total' [] n = n
total' (x:xs) n = total' xs $! (n+x)
total xs = total' xs 0

-- ! 模式是强制在参数匹配前来计算参数的值，! 模式需要在文件首处声明 {-# LANGUAGEBangPatterns #-}
-- 语言扩展，而 ($!) 则为在调用函数时计算参数的值。这些内容将在惰性求值一章中介绍。

-- 互调递归（mutual recursion）是一种特殊的情形，即两个函数的定义分别都用到了对方。
-- 比如，even 函数的定义用到了 odd 函数，odd 函数的定义也用到了 even 函数。
-- even 0 = True
-- even n = odd (n - 1)
-- odd 0 = False
-- odd n = even (n - 1)


-- 麦卡锡的 91 函数由 Lisp 的发明者——约翰 • 麦卡锡（John McCarthy）2引入。
mc n 
    | n > 100 = n - 10
    | otherwise = mc $ mc $ n + 11
;

-- 斐波那契数列由伟大的意大利数学家莱昂纳多·斐波那契（Leonardo Pisano Fibonacci）
-- 引入。这是一个似乎具有魔幻色彩的数列，有很多惊奇的性质。比如，前一个数与后一个数
-- 的比值逼近黄金分割值

fibStep :: Num a => (a, a) -> (a, a)
fibStep (u, v) = (v, u+v)

fibPair :: (Eq a, Num a) => a -> (a, a)
fibPair 0 = (0, 1)
fibPair n = fibStep (fibPair (n-1))

fastFib :: (Eq a, Num a) => a -> a
fastFib n = fst (fibPair n)

fibs :: (Enum a, Eq a, Num a) => a -> [a]
fibs n = map fastFib [1..n]
