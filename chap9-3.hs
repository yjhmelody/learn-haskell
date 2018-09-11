{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative
import Data.Monoid

-- 类型类实例的实现


-- deriving 关键字

-- 对于使用 data 或者 newtype 定义的类型，Haskell 98 标准中规定它们可以导出的类型
-- 类 Eq、Ord、Enum、Ix、Bounded、Read 与 Show 类型类的实例，但是在导出不同的类型类时，
-- 类型的定义需要满足特定的要求
-- GHC 还可以自动导出 Functor、Foldable、Traversable、Typeable、Data、Generic 类型类


-- instance 关键字

data Shape = Circle Double | Square Double | Rectangle Double Double

instance Eq Shape where
    Circle r1 == Circle r2 = r1 == r2
    Square l1 == Square l2 = l1 == l2
    Rectangle a1 b1 == Rectangle a2 b2 = a1 == a2 && b1 == b2
    _ == _ = False

-- GHC 提供了 InstanceSigs语言扩展来让我们给出类型类中函数的签名：
-- {-# LANGUAGE InstanceSigs #-}
-- instance Eq Shape where
    -- (==) :: Shape -> Shape -> Bool
    -- Circle r1 == Circle r2 = r1 == r2
    -- ...


-- 由于 Haskell 标准中规定类型类中类型变量在实现时也必须使用类型变量去匹配，即写成：
-- instance Eq (Shape a)

-- 而我们希望在实现相等类型类实例时使用Eq (a,a) 而非 Eq a，虽然在这种情形上意义并不大，
-- 但是在很多时候还是需要的，此时就需要 FlexibleContext语言扩展来打破这一限定：
-- {-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- data Shape a = Circle a | Square a | Rectangle a a
-- instance Eq (Shape Double) -- 只需要FlexibleInstances
-- instance Eq (a,a) => Eq (Shape (a,a)) -- 同时需要两个扩展


-- 空 instance 与 DeriveAnyClasses 语⾔扩展

class MyShow a where
    myshow :: a -> String
    myshow _ = "default"

data Person = P String Int
instance MyShow Person

-- DeriveAnyClass语言扩展，这个扩展可以允许我们使用 deriving 关键字导出有默认实现的类型类实例
-- {-# LANGUAGE DeriveAnyClass #-}
-- ...
-- data Person = P String Int deriving MyShow


-- newtype 定义类型的类型类实例导出

-- GHC 给出了一个语言扩展GeneralizedNewtypeDeriving来帮助我们完成这件事，它能为一般的 newtype 类型类导出实例。
-- 对于我们自己定义的类型类这一扩展也是可以使用的。

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype A a b = A (a,b) deriving (Show, Functor, Applicative)
-- > fmap (+1) (A (1,2))
-- A (1,3)
-- > pure (5 :: Int) :: A (Sum Int) Int
-- A (Sum {getSum = 0},5)


-- 为类型的别名实现类型类的实例

-- GHC 提供的 TypeSynonymInstances语言扩展可以让我们使用 type 关键字定义的类型声明为类型类实例
class ToInt i where
    toInt :: i -> Int

type P = Int
instance ToInt P where
    toInt p = p


-- 开启了 FlexibleInstances会意味着开启 TypeSynonymInstances
type T a = (a, a)
instance ToInt (T Int) where
    toInt (a, b) = a


-- 独⽴的类型类实例导出

-- A.hs
-- module A where
-- data Foo a = Foo a

-- B.hs
-- {-# LANGUAGE StandaloneDeriving #-}
-- module B where
-- import A
-- deriving instance (Show a) => Show (Foo a)

-- 我们把 Foo 的定义与导出 Show 类型类实例的过程分离了。开启 StandaloneDeriving时，使用 deriving instance 
-- 再跟导出的类型类与类型就可以了，基本跟 deriving 的使用是一样的，不同的是我们需要手动给出类型上下文。
