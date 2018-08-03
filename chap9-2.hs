{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
    {-# MINIMAL (==) | (/=) #-}


-- 有序类型类 Ord
class (Eq a) => Ord a where
    compare ::  a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min :: a -> a -> a

    compare x y | x == y = EQ
                | x <= y = LT
                | otherwise = GT

    x < y   = case compare x y of {LT -> True; _ -> False}
    x <= y  = case compare x y of {GT -> False; _ -> True}
    x > y   = case compare x y of {GT -> True; _ -> False}
    x >= y  = case compare x y of {LT -> False; _ -> True}
    
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
    {-# MINIMAL compare | (<=) #-}


    -- 有界类型类 Bounded
class Bounded a where
    minBound :: a
    maxBound :: a


    -- 枚举类型类 Enum
class Enum a where 
    toEnum          :: Int -> a
    fromEnum        :: a -> Int
    succ, pred      :: a -> a
    enumFrom        :: a -> [a] -- [n..]
    enumFromThen    :: a -> a -> [a] -- [n,n'..]
    enumFromTo      :: a -> a -> [a] -- [n..m]
    enumFromThenTo  :: a -> a -> a -> [a] -- [n,n'..m]
    -- 一般来说，只需要定义 toEnum 与 fromEnum 就算实现 Enum 类型类了，
    -- 这样就在 Int 与该类型间做了一个映射。


    -- 索引类型类 Ix
class (Ord a) => Ix a where
     {-# MINIMAL range, (index | unsafeIndex), inRange #-}
     range                  :: (a, a) -> [a]
     index                  :: (a, a) -> a -> Int
     GHC.Arr.unsafeIndex    :: (a, a) -> a -> Int
     inRange                :: (a, a) -> a -> Bool
     rangeSize              :: (a, a) -> Int
     GHC.Arr.unsafeRangeSize:: (a, a) -> Int

-- 1 > :m +Data.Ix
-- 2 > range (LT,GT)
-- 3 [LT,EQ,GT]
-- 4 > data Weekday = Mon | Tue | Wed | Thu | Fri | Sat |Sun deriving (Eq,Ord,Ix,Show)
-- 5 > range (Mon, Sun)
-- 6 [Mon,Tue,Wed,Thu,Fri,Sat,Sun]

-- 1 > index (Mon, Thu) Fri
-- 2 *** Exception: Error in array index
-- 3 > index (Mon, Thu) Wed
-- 4 2

data MyNum = One | Two | Three

instance Show MyNum where
    show One = "1"
    show Two = "2"
    show Three = "3"

-- 除定义 show 函数外还可以有另外一种选择，就是定义 showsPrec 函数
-- showsPrec :: Show a => Int -> a -> ShowS

newtype Identity a = Identity {runIdentity :: a}

instance (Show a) => Show (Identity a) where
    showsPrec d (Identity x) =
        showParen (d > 10) $ showString "Identity " . showsPrec 11 x


-- 函⼦类型类 Functor

-- 函子类型类中仅仅定义了一个函数 fmap，意为给定类型 a 与类型 b 间有一个映射，可以返回另一
-- 个参数化类型上的映射，这个参数化类型的 kind 必须为 * -> *。

class Functor f where
    fmap :: (a -> b) -> a -> b

(<$>) :: Functor f => (a -> b) -> a -> b
(<$>) = fmap

newtype Container a = Container a

instance Functor Container a where
    fmap f (Container a) = Container (f a)

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- 函数也是构造出的类型，(->) r 也为一个函子。这里，读者可能有些疑惑了，(->) r 类型其
-- 实本质与其他的参数化类型并无不同。

instance Functor ((->) r) where
    fmap f g = (\x -> f (g x)) -- 或用η化简写作fmap f g = f $ g

-- 在这种情况下，整个 fmap 类型为 (a -> b) -> ((->) r a) -> ((->) r b)，我们可以将->
-- 写为中缀符号 (a -> b) -> (r -> a) -> (r -> b)
-- 可以很清楚地看到，fmap 实际上是将函数 f 与 g 复合了，这个函子类型类实例的声明就可以写成：

instance Functor ((->) r) where
    fmap = (.)

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq, Functor)


-- 在实现时函子类型类时要满足下面的定律，这是需要我们来保证的，函子在数学上要符合以下的定律：
-- 定律 9.2.1. fmap id = id
-- 这条定律说明这函子 f 这种结构上需要保留恒值映射

-- 定律 9.2.2. fmap (f ◦ g) = fmap f ◦ fmap g
-- 这条定律则说明 fmap 函数在复合函数运算符上是服从分配律的。

-- 可应⽤函⼦ Applicative

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f a

-- Applicative 类型类除了具有 Functor 的特性以外，能做的只是调用函子容器内的函数。因
-- 此，常常把这个类型类称作可应用函子 Applicative Functor，意为可应用函数的函子，如
-- Maybe 类型。

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> arg = fmap f arg

-- 为了更方便的表达这种计算，GHC 的库中定义了 liftA、liftA2、liftA3 函数，lift 意为
-- 抬升，意思是将一个函数运算的参数分别放置于实现了 Applicative 类型类的容器中
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

(*>) :: f a -> f b -> f b
u *> v = pure (const id) <*> u <*> v

(<*) :: f a -> f b -> f a
u <* v = pure const <*> u <*> v

-- 在使用 *> 运算符时，先计算第一个参数并忽略其结果（这里注意忽略结果并不等于计算没
-- 有意义），然后再计算第二个参数但将其返回为结果，<* 则刚好相反。用户也可以很容易地
-- 使用 liftA2 函数来定义它们，然后应用 η 化简。
(*>) = liftA2 (const id)
(*<) = liftA2 const

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- [(*),(+)] <*> [1,2] <*> [3,4]
-- [3,4,6,8,4,5,5,6]


-- 可应用函子 Applicative 是基于 Functor 的，也就是说它的下面有 3 个函数：
-- 1 fmap0 :: a -> f a -- pure
-- 2 fmap1 :: (a -> b) -> f a -> f b -- fmap
-- 3 (<*>) :: f (a -> b) -> f a -> f b

-- fmap0 与 fmap1 有一个序，很好理解，fmap0 的第 1 个参数是一个 0 元函数，而 fmap1 的
-- 第 1 个参数是 1 元函数，然后我们这里定义一个 MultiFunctor 类型类去试着定义 fmap2、
-- fmap3 等等：
import Control.Applicative

class MultiFunctor f where
    fmap0 :: a -> f a -- pure
    fmap1 :: (a -> b) -> f a -> f b -- fmap
    fmap2 :: (a -> b -> c) -> f a -> f b -> f c
    -- 有了前面的3个函数就可以定义任意多元的fmap函数了
    fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    fmap3 h x y z = fmap2 ($) (fmap2 h x y) z
    fmap4 :: (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
    fmap4 h w x y z = fmap2 ($) (fmap3 h w x y) z
    fmap5 :: (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
    fmap5 h v w x y z = fmap2 ($) (fmap4 h v w x y) z

-- 后面的 fmap3、fmap4、fmap5 以及更多元的映射函数 fmap 都可以用 fmap2 与少 1 元的 
-- fmap 函数来定义，即 fmapn f a1 a2 · · · an = fmap2 ($) (fmap(n−1) f a1 a2 · · · an−1) an。

-- 我们就可以看到这里定义的多函子类型类 MultiFunctor 本质上就是可应用函子
-- Applicative 类型类，它的本质就是可以把任意多元的函数应用到任意多个放在 f 容器里
-- 的参数上，把一般的函数应用提升到了某种参数化的类型 f 上，比如 Maybe、列表等等。
-- Control.Applicative 模块中定义的 liftA、liftA2、liftA3 对应的就是第 1 个参数为 1 元、
-- 2 元和 3 元函数的 fmap，lift 就是提升的意思。

(<<*>>) :: f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
(<<*>>) = liftA2 (<*>)
-- > Just [(+1), (+6)] <<*>> Just [1, 2]
-- Just [2, 3, 7, 8]


-- 选择可应⽤函⼦ Alternative
-- Alternative 意为可二选一的、后备的。这个类型类依赖于 Applicative
-- 类型类，并且在 Control.Applicative 中是这样定义的

infixl 3 <|>

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

-- 这个类型类中定义的运算符 <|> 意为其他的选择。比如，当给定多个值的时候，需要选择合
-- 理的，如下：
-- > Nothing <|> Nothing <|> Just 1 <|> Just 2
-- Just 1

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> p = p
    Just x <|> _ = Just x

-- 通过上面的函数计算与 <|> 的定义，应该可以看出，empty 定义的为 <|> 运算符的单位
-- 元，正如 <*> 运算符的单位元为 pure id 一样。

-- 列表类型实现 Alternative 类型类实例时，<|> 运算符应该与 ++ 运算符是等价的。
instance Alternative [] where
    empty = []
    (<|>) = (++)
    some :: f a -> f [a]
    some v = some_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v

    many :: f a -> f [a]
    many v = many_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v
        
    optional :: Alternative f => f a -> f (Maybe a)
    optional v = Just <$> v <|> pure Nothing

-- Alternative 类型类中还定义好了另外两个函数 some 与 many 定义，使用了 Applicative
-- 的 <*> 与 Alternative 的 <|> 运算符

-- some 函数先检察 v 值是否为 empty
-- 单位元，如果为单位元，那么返回这个单位元，否则返回 v 内的值多次，即尝试返回 v 内的
-- 值 1 次或者更多次。而 many 函数先判定 v 值是否为 empty 单位元，如果为单位元，那么返
-- 回 []，意为返回 v 内的值 0 次或者更多次。而定义中的本地函数 many_v 与 some_v 为互调
-- 递归定义，若 some_v 不能返回 1 次以上，则返回 []，这正是 many 的意思。同理，将 v 内
-- 的值返回一次，然后再返回多次也正是 some_v 的意思。

-- 如果在 GHCi 中运行 some (Just 5)，那么函数的运行不会停止。因为 5 为一
-- 个纯数值，所以它的计算在 Haskell 中永远不会失败，其结果可以理解为 Just [5,5,5,5..]。
-- many (Just 5) 的结果就与 some (Just 5) 相同了，它从 Just 5 中一直取 5 而不会停止。

-- 库中还定义了 optional 函数，即返回 v 中的值 0 次或者 1 次，如果没有值，则会返回f Nothing。
-- > optional [1,2,3]
-- [Just 1,Just 2,Just 3,Nothing]


-- 上面的函子、可应用函子、可选择应用函子 3 个类型类是来自于抽象代数的