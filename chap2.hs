type RGB = (Int, Int, Int)
type Picture = [[RGB]]

-- type 关键字只能做一些替换。在定义类型的别名时，它只是将已经有的数据类型组合成一个
-- 新的数据类型，并不能构造出新的数据类型。

add, sub :: Int -> Int -> Int
add a b = a + b
sub a b = a - b

f :: Num a => a -> a
f x = 4 * x + 1

area r = pi * r ^ 2

f' :: Num a => (a, a) -> a
f' (x, y) = 4 * x + 5 * y + 1
-- f'=\x y -> 4*x + 5*y + 1

-- lambda
g :: Int -> Int -> Int
g = \y -> \x -> (+) x y
-- g = \x -> (+) x
-- g = (+)


-- 海伦公式（Heron’s formula）
s :: Double -> Double -> Double -> Double
s a b c = let p = (a + b + c) / 2
    in sqrt(p * (p - a) * (p - b) * (p - c))
;
-- 多个表达式绑定可以用分号隔开

-- 除了用 let..in.. 以外，还可以使用 where


s' :: Double -> Double -> Double -> Double
s' a b c = sqrt(p * (p - a) * (p - b) * (p - c))
    where p = (a + b + c) / 2
;

-- > let x = 6 in x * let x = 2 in x * x
-- 2 24

-- 条件表达式
isTwo :: Int -> Bool
-- isTwo n = if n == 2 then True else False
isTwo = (==2)


-- 情形分析表达式是用 case 与 of 关键字来对一个类型中不同的值进行讨论的，它与顺
-- 序式编程里的 switch .. case 类似。
month :: Int -> Int
month n = case n of
    1 -> 31
    2 -> 28
    3 -> 31
    4 -> 30
    -- ...
    12 -> 31
    _ -> error "invalid month"

-- 守卫表达式（guarded expression）是使用 | 将函数的参数按特定的条件分开，| 像一个
-- 守卫一样，如果不能满足条件，它绝不会让不符合条件的表达式运算。不同条件的守卫表达
-- 式的 | 需要对齐。
abs' :: (Num a, Ord a) => a -> a
abs' n
    | n > 0 = n
    | otherwise = -n

-- otherwise == True

-- 模式匹配（pattern match）中的 pattern 理解为模式，它指的是一个类型的值或者定义
-- 成的形式。
month' :: Int -> Int
month' 1 = 31
month' 2 = 28
month' 3 = 31
month' 4 = 30
-- ...
month' 12 = 31
month' _ = error "invalid month"

-- case..of.. 和模式匹配是对于类型值不同形式的分析，所以在一定意义上它们
-- 是等同的，而用竖线的守卫表达式和 if..then.. else.. 都是对参数的条件讨论的表达式，
-- 所以它们在一定程度上是等同的。

head' [] = error "empty list"
head' (x:_) = x


-- 运算符可能有 3 种属性，即优先级（precedence）、结合性（associativity）和位置（fixity）
-- 。在 Haskell 中，运算符号有 0 ∼ 9，共十个优先级。结合性又分为左结合性、右结合性、无
-- 结合性。根据位置又可分为前缀、中缀和后缀运算符，一般使用的函数常常可以理解为前缀
-- 运算符。

-- 定义的函数默认有着最高的优先级，
-- 比 9 还高，并且为左结合。例如，f g h i 意为 (((f g) h) i)。

($) :: (a -> b) -> a -> b
f $ x = f x

-- 在 Haskell 中，可以自由地定义自己的运算符号，但要声明它的结合性是
-- 怎样的，优先级是多少。声明运算符的关键字有三个：infixl 为左结合，infixr 是右结合，
-- infix 为无结合。定义时先用这三个关键字说明结合性，再声明优先级，最后给出运算符的
-- 符号。如果需要定义多个结合性与优先级相同的运算符，那么它们之间要用逗号分开:

-- 定义右结合的运算符
infixr 5 <->, <+>
(<->), (<+>) :: Int -> Int -> Int
(<->) x y = x - y
(<+>) x y = x + y

-- > 5 <-> 2 <-> 2
-- 5

infixr 4 `foo`
foo a b = a + b

-- 在 GHCi 中定义函数使用模式匹配一定要写为多选的形式，此时在 GHCi 中还可以使用:{来头，以:} 结尾来定义多行函数
-- :{
-- let { foo :: Int -> Int
-- ; foo 1 = 2
-- ; foo 2 = 1
-- ; foo n = 0 }
-- :}