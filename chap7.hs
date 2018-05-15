-- c⾼阶函数（higher order function）

-- 定义 7.0.1. 高阶函数：以其他函数为参数的函数，或者以函数作为结果返回的函数称为高阶函数。
-- 评论 7.0.1. 由于以函数作为返回结果只是柯里化函数的特点而已，所以高阶函数常常仅特指那些以函数为参数的函数。

-- 我们知道非柯里化函数与柯里化函数等价的，那么它们之间则是可以互相转化的，
-- 其中转化的函数的 curry 与 uncurry 很明显都是高阶函数。

-- 高阶函数常常是某一些有共同特点的函数更为一般的形式。
power2 :: Num a => [a] -> [a]
power2 [] = []
power2 (x:xs) = x ^ 2 : power2 xs

plus1 :: Num a => [a] -> [a]
plus1 [] = []
plus1 (x:xs) = (x + 1) : plus1 xs

-- map 则是可以表达 plus1 与 power1 这一类函数的更为一般的函数。
-- 根据不同的需要我们可能会映射不同的函数 f，这一点需要我们在应用 map 函数时指
-- 定，所以还应该把 map 函数中的 f 作为参数引入，这样 map 的定义就变换成
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- 之前在6.7中定义的很多种不动点函数，它们都是高阶函数
fix1 :: (a -> a) -> a
fix1 f = f (fix1 f)

fix2 :: Eq a => (a -> a) -> a -> a
fix2 f x    | x == f x = x
            | otherwise = fix2 f (f x)

fix3 :: (t -> t -> Bool) -> (t -> t) -> t -> t
fix3 c f x  | c x (f x) = x
            | otherwise = fix3 c f (f x)

-- 首先看它们的类型，都是需要类型为 (a -> a) 作为参数的函数，其中，fix3 还需要一
-- 个判定停止条件的函数，在牛顿法开方时用以控制精度。这一类函数就是为了应用一个函数
-- 多次而编写的更为一般的函数。
apply :: (a -> a) -> Int -> a -> a
apply f 0 x = x
apply f n x = apply f (n-1) (f x)

-- 折叠函数 foldr 与 foldl


-- 右折叠函数 foldr

-- 常常，用户需要对一个列表中的元素做一个二元运算符的操作。当列表为空时，返回一个特殊的值。
sum' [] = 0
sum' (x:xs) = x + sum' xs

and' [] = True
and' (x:xs) = x && and' xs

-- 显然以上两个函数有着公共的部分，即对于空列表返回一个特殊值，
-- 对于非空列表则把首元素与递归调用尾部列表产生的结果做一个二元运算，
-- 如果其中的二元运算与特殊值给定了，就可以写出这个函数的框架：

-- foldr ⊕ e [x1,x2,x3 ... xn] 是在计算 x1⊕(x2⊕(x3... (xn⊕e)...) 的值，
-- 这里 ⊕ 必须为右结合的二元运算符，它的类型需要为 a -> b -> b 形式，并且在很多情况下，
-- e 是⊕ 的右单位元。
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- sum' xs = foldr' (+) 0 xs
-- and' xs = foldr' (&&) True xs

-- (a -> b -> b) -> b -> [a] -> b 是 foldr 在 GHC 7.8 之前的类型，
-- 在 GHC 7.10 后它的类型是：foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

(+++) :: [a] -> [a] -> [a]
(+++) = flip (foldr (:))

-- 若列表首元素与第一个参数相同则不会插入，否则与: 函数相同
skip :: Eq a => a -> [a] -> [a]
skip x [] = [x]
skip x (y:ys) = if x == y then y:ys else x:y:ys

-- 将一个有多个连续的元素的列表压缩为一个没有连续相同元素的列表
compress :: Eq a => [a] -> [a]
compress = foldr skip []

snoc :: a -> [a] -> [a]
snoc x = foldr (:) [x]

reverse' xs = foldr snoc [] xs

concat' :: [[a]] -> [a]
concat' = foldr (++) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x xs -> f x : xs) []


-- 左折叠函数 foldl

-- 如果这个二元运算是左结合的，可以用 foldl 来计算它，foldl 函数是这样定义的
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs
-- foldl 是尾递归定义的
-- foldl ⊕ e [x1,x2,x3...xn] 是在计算 (..((e⊕x1) ⊕ x2)...)⊕ xn 的值，这里，
-- ⊕ 必须为左结合的二元运算符，它的类型应该满足 (a-> b -> a)，常常 e 为⊕ 运算符的左单元。

-- 但是，由于惰性求值，这样的尾递归定义还是会消耗过多的存储单元，可以使用 foldl
-- 的严格求值版本 foldl'，它在 Data.List 模块中

reverse'' :: [a] -> [a]
reverse'' = foldl' (flip (:)) []

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)
foldr1' _ _ = error "foldr1 empty list"

unwords' :: [String] -> String
unwords' [] = ""
unwords' ws = foldr1 (\w s -> w ++ ' ':s) ws

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [x] = x
foldl1' f (x:xs) = foldl' f x xs
foldl1' _ _ = error "foldl1 empty list"

maximum', minimum' :: Ord a => [a] -> a
maximum' = foldl1' max
minimum' = foldl1' min

-- 求一列数的最大公约数函数和最小公倍数的函数
gcds = foldr1' gcd
lcms = foldr1' lcm

-- scanl 与 scanr 函数同 foldl 与 foldr 函数相似，但是它会记录二元函数计算的中间结果。
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ a [] = [a]
scanl' f a (x:xs) = a : scanl' f (f a x) xs

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ a [] = [a]
scanr' f a (x:xs) = f x y : ys
    where ys@(y:_) = scanr' f a xs