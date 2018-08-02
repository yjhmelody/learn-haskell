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

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> arg = fmap f arg

