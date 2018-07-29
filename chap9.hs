{-# language MultiParamTypeClasses #-}


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