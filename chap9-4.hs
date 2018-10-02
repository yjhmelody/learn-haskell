-- Haskell 中其他常⻅的类型类

-- 单位半群类型类 Monoid

-- 对于集合 S 和基于 S 的二元运算 ⊕ : S × S → S，若 ⊕ 满足结合
-- 律，即 ∀x, y, z ∈ S 都有 (x ⊕ y) ⊕ z = x ⊕ (y ⊕ z)，有序对 (S, ⊕) 被称为半群，倘若在半群
-- (S, ⊕) 上还存在一个单位元 1，那么这三元组 (S, ⊕, 1), 为组成的代数结构就称为单位半群
-- （monoid），也称作幺半群

{-# LANGUAGE NoImplicitPrelude #-}

module Class where
import Prelude hiding (mempty, mappend)

class MyMonoid a where
    mempty :: a
    mappend :: a -> a -> a


-- 常见的单位半群：(Bool, &&, True) 与 (Bool, ||, False)
newtype All = All {getAll :: Bool}
    deriving (Eq, Ord, Read, Show, Bounded)

instance MyMonoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)

newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance MyMonoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

-- (Z, ×, 1) 与 (Z, +, 0) 也均为单位半群
newtype Product a = Product {getProduct :: a}
    deriving(Eq, Ord, Read, Show, Bounded)

instance Num a => MyMonoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

newtype Sum a = Sum {getSum :: a}
    deriving(Eq, Ord, Read, Show, Bounded)

instance Num a => MyMonoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

-- 列表也是一个单位半群，它的二元运算符为 ++，即字符串拼接（concatenation）运算符
instance MyMonoid [a] where
    mempty = []
    mappend = (++)

-- (f:: a ->a, (.) , id) 也为单位半群
-- 名为 Endo 是由于代数上把 a -> a 这种同种类型间的映射称为函数自同态（endomorphisms）
newtype Endo a = Endo {appEndo :: a -> a}
instance MyMonoid (Endo a) where
    mempty = Endo id
    Endo f `mappend` Endo g = Endo (f . g)

-- Endo 的对偶
newtype FunApp a = FunApp { appFunApp :: a -> a}
instance MyMonoid (FunApp a) where
    mempty = FunApp id
    FunApp f `mappend` FunApp g = FunApp (g . f)


instance MyMonoid () where
    mempty = ()
    mappend _ _ = ()

instance (MyMonoid a, MyMonoid b) => MyMonoid(a, b) where
    mempty = (mempty, mempty)
    (a1, b1) `mappend` (a2, b2) = (a1 `mappend` a2, b1 `mappend` b2)

-- 如果 Maybe 的类型参数为是单位半群，那么 Maybe a 也满足单位半群的条件
instance MyMonoid a => MyMonoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- 对于一个单位半群，它总是存在一个对偶（dual），这个对偶也是单位半群，对偶的意思
-- 就是说单位半群总是成对出现的，两个单位半群唯一的区别就在于一个的 mappend 函数的参数被颠倒了
newtype Dual a = Dual { getDual :: a} 
    deriving (Show)
instance MyMonoid a => MyMonoid (Dual a) where
    mempty = Dual mempty
    Dual x `mappend` Dual y = Dual (y `mappend` x)

-- 也就是说对于一个单位半群 a，总是可以得到另外一个单位半群 Dual a
-- 由于 Product 与 Sum 还有 Any 与 All 类型类的二元运算是满足交换律的，所以它们的对偶
-- 与自身是等价的。实际上，对于一个单位半群，只需要把它的二元运算 mappend 更改成 flip
-- mappend 就得到了另一个。


-- 半群类型类 Semigroup

-- 如果单位半群上没有单位元，这个代数结构就是半群，也就是只闭合于一个有结合性质
-- 的二元运算之下的集合 S 构成的代数结构，即有序对 (S, ⊕) 被称为半群。对于所有的单位半群，都满足这个条件。

-- 除了半群 Semigroup 的定义以外，Hackage 上的 group 库还提供了群（Group）类型类，
-- 只是在单位半群的基础上多了一个逆函数（inverse function），此外还提供了可交换群（abelian group）类型类，
-- 可交换群的类型类实例中定义的二元运算符必须满足交换律


-- 默认值类型类 Default

class Default a where
    def :: a

instance Default Int where 
    def = 0

instance (Default r) => Default (e -> r) where
    def = const def

instance Default (Maybe a) where
    def = Nothing

instance Default Any where
    def = mempty

instance Default All where
    def = mempty

-- 与半群类似，对于一个单位半群它总是可以有一个默认的值 mempty
