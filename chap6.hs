-- 列表内包

-- 列表内包（list comprehension）的内包（comprehension）一词在这里意为列表内涵、内
-- 包1，具体指的是 Haskell 提供一种精简的语法来对列表中的元素进行处理，如匹配、过滤、
-- 应用函数或者组合，这种表达形式源于数学中定义集合的符号表示，这并不是 Haskell 的新
-- 功能，而仅仅只是语法糖。

-- > [x^2|x <- [0..100], x < 10]
-- [0,1,4,9,16,25,36,49,64,81]

-- 其中 x<-[0..100] 为生成器
-- 而 x < 10 则是对 x 的条件限制
-- x^2 则对符合条件的 x 进行转换
-- 生成列表的时候可以使用多个生成器，但是生成的过程是有一定的顺序的，写在后边的生成器将会被先遍历。

-- > [(x,y) | x <- [1..4], y <- [1..2]]
-- [(1,1),(1,2),(2,1),(2,2),(3,1),(3,2),(4,1),(4,2)]

map' f xs = [f x | x <- xs]

filter' f xs = [x | x <- xs, f x]

-- 当然，限定的条件可能不止一个，可以再加入一些限定的条件，这些限定条件之前是逻辑与的关系。

-- > [x| x <- [0..], even x, x > 10]
-- [12,14,16,18,20,22,24,26,28,...

-- 使用列表内包时，如果用不到生成器中产生的值，可以使用通配符来匹配。
length' xs = sum [1 | _ <- xs]

-- 使用列表内包可以很容易地表达顺序式语言中的循环。
series :: Int -> [Double]
series n = [1 / (2 * (fromIntegral k) + 1) * (-1) ^ k | k <- [0..n]]

-- 接下来，可以使用这个公式来粗略地计算 π 的值。由于这个公式收敛较慢，要应用 20 万次
-- 以上才能精确到第 5 位小数，所以并不是什么高效的方法。
-- > 4 * (sum $ series 100000)
-- 3.1416026534897203

-- 6.1.1
-- series' :: Int -> [(Double, Double)]
-- series' n = [
--     (
--         1 / (2 * (fromIntegral k) + 1) / ((-2) ^ (2 * (fromIntegral k) + 1)),
--         1 / (2 * (fromIntegral k) + 1) / ((-3) ^ (2 * (fromIntegral k) + 1))
--     )
--     | k <- [0..n]
--     ]


-- 并列的列表内包（parallel list comprehension）是列表内包的一个扩展。
-- > :set -XParallelListComp
-- > [(x,y) | x <- [1 ,2, 3] | y <- [4, 5, 6]]
-- [(1,4),(2,5),(3,6)]

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]

isPrime' :: Integral a => a -> Bool
isPrime' 2 = True
isPrime' p = p > 1 && (all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) [3, 5..])

-- 在加密的时候，常常需要生成非常大的素数，现在可以写一个函数，以一个任意大的正
-- 整数作为参数，称为“种子”，然后返回大于或等于它并且距离最近的素数。思路是这样的，
-- 若这给定的这个“种子”为奇数，先来判断它本身是否为素数，若为素数直接返回即可，否
-- 则需要递归地判断这个数加 2 是否为素数。若这个“种子”是偶数，递归地判断这个数加 1
-- 是否为素数即可。
nextPrime :: Integer -> Integer
nextPrime a | odd a = if isPrime a then a else nextPrime (a+2)
            | otherwise = nextPrime(a+1)

-- 埃拉托斯特尼筛法（Eratosthenes sieve）

-- 给定从 2 开始连续的一列数，2 为素数。那么，在 2 之后有 2 为因数的数均不为素数，
-- 可以被筛掉。下一个数为 3，3 之后的所有为 3的倍数的数就全被筛掉了，
-- 因为 4 为 2 的倍数，这在第一轮中已经被筛掉了，下一个是 5，依次类推，
-- 假定列表是无穷的，那么按着这个方法可以遍历所有的素数。
sieve :: (Integral a) => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

prime' = sieve [2..]