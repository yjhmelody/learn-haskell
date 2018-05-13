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

golden :: Fractional a => Int -> [a]
golden n = take n (map (\(x, y) -> x/y) (iterate fibStep (0, 1)))
-- > golden 7
-- [0.0,1.0,0.5,0.6666666666666666,0.6,0.625,0.6153846153846154]

combine :: [(a, a)] -> [(a, a, a)]
combine ((f1, f2):(f3, f4):fs) = (f1, f2, f4) : combine ((f3, f4):fs)
combine _ = []
-- combine $ fibPairs 7
-- [(1,1,2),(1,2,3),(2,3,5),(3,5,8),(5,8,13),(8,13,21)]

fibPairs :: Int -> [(Int, Int)]
fibPairs n = map fibPair [1..n]

difference :: Int -> [Int]
difference n = map (\(f1, f2, f3) -> f1 * f3 - f2 * f2) (combine $ fibPairs n)
-- > difference 10
-- [1,-1,1,-1,1,-1,1,-1,1]


romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

romeAmount :: [Int]
romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

romePair :: [(Int, String)]
romePair = zip romeAmount romeNotation

-- 给定一个十制数，只需要找到第一个不大于它的罗马数字，然后从十进制数字中减去，再将剩下的十进制数递归地转换成罗马数字即可。
subtrahend :: Int -> (Int, String)
subtrahend n = head (dropWhile (\(a, _) -> a > n) romePair)
-- > subtrahend 5
-- (5,"V")
-- > subtrahend 86
-- (50,"L")

convert :: Int -> String
convert 0 = ""
convert n = let (rome, m) = subtrahend n
            in m ++ convert(n - rome)
-- > convert 12
-- (0,"XII")
-- > convert 109
-- (0,"CIX")
-- > convert 1925
-- (0,"MCMXXV")


binSearch :: (Ord a) => a -> [a] -> Bool
binSearch a [] = False
binSearch a xs  | m < a = binSearch a behind
                | m > a = binSearch a front
                | otherwise = True
                where (front, m:behind) = splitAt (length xs `div` 2) xs

-- 5.8.1
binSearch2 :: (Ord a) => a -> [a] -> [a]
binSearch2 a [] = []
binSearch2 a xs | m < a = binSearch2 a behind
                | m > a = binSearch2 a front
                | otherwise = checkNeibor m (reverse front) ++ [m] ++ checkNeibor m behind
                where (front, m:behind) = splitAt (length xs `div` 2) xs

checkNeibor :: (Ord a) => a -> [a] -> [a]
checkNeibor n [] = []
-- checkNeibor n (x:xs) = if n == x then x : checkNeibor n xs else []
checkNeibor n xs = takeWhile (\x -> x == n) xs

-- > binSearch2 4 [1,2,2,3,4,4,4,5,6,7,8]
-- [4,4,4]


-- insertion sort
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y: insert x ys

insertionSort :: Ord a => [a] -> [a] -> [a]
insertionSort xs [] = xs
insertionSort xs (y:ys) = insertionSort (insert y xs) ys

insertionSort' :: Ord a => [a] -> [a]
insertionSort' [] = []
insertionSort' (x:xs) = insert x (insertionSort' xs)

-- bubble sort
swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
    | x1 > x2 = x2 : swaps (x1:xs)
    | otherwise = x1 : swaps (x2:xs)

-- 来定义一个不动点函数。这个函数可以一直调用swaps，一直到列表不再发生变化为止
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
    where x' = f x

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = fix swaps xs

bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' xs  | swaps xs == xs = xs
                | otherwise = bubbleSort' $ swaps xs

bubbleSort'' :: Ord a => [a] -> [a]
bubbleSort'' [] = []
bubbleSort'' xs = bubbleSort'' initialElements ++ [lastElement]
    where   swappedxs       =  swaps xs
            initialElements = init swappedxs
            lastElement     = last swappedxs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
    -- twice lookup
    where   mini = minimum xs
            xs' = delete mini xs
    
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
    where   left = filter (<x) xs
            right = filter (>=x) xs

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit _ [] = ([], [])
filterSplit f (x:xs)| f x = ((x:l), r)
                    | otherwise = (l, (x:r))
    where (l, r) = filterSplit f xs

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' [x] = [x]
quicksort' (x:xs) = quicksort' l ++ [x] ++ quicksort' r
    where (l, r) = filterSplit (<x) xs


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | x > y = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)
                
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort x1) (mergeSort x2)
    where 
        (x1, x2) = halve xs
        halve xs = (take l xs, drop l xs)
        l = (length xs) `div` 2 

