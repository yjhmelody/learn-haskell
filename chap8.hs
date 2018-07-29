{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}


import Data.Sequence
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)


-- 定义数据类型
-- data 是定义新类型的关键字，后边跟想要定义的类型名称，类型名称首字母要大写。
data Bool' = False' | True' deriving Show

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq, Ord, Enum, Read)

-- 这里可以先使用 deriving 关键字来让这个类型自动实现一些类型类，
-- 比如 Show、Eq、Enum、Ord 等。

-- 实现了相等类型类 Eq，所以可以比较两个值是否相等

-- 除了相等类型类 Eq，还实现了基于它的有序类型类 Ord，这样就可以比较这些值之间的大小

-- 由于有序类型类 Ord 是基于相等类型类 Eq 的，因此一定要实现相等类型类 Eq 后才能实现
-- 有序类型类 Ord。

-- 由于使用 deriving 还实现了 Enum 类型类，因此可以使用.. 遍历定义的类型中的值


-- 其实，实现了枚举类型
-- 类 Enum 后，有很多函数可以立即使用，如 succ、pred 及其他的函数，这里简单介绍 succ
-- （successor 的简写）函数与 pred（predecessor 的简写）函数。succ 函数会返回参数的下一
-- 个枚举类型的值，而 pred 会返回给定参数的前一个枚举类型的值。如果给定的参数已经为
-- 边界值，则会出现异常，但这种异常很好处理。“明天”函数可以这样定义

tomorrow Sun = Mon
tomorrow d = succ d

yesterday Mon = Sun
yesterday d = pred d


-- 除了以上的 4 个比较常用的类型类，还有可读类型类 Read，即可以使用 read 函数来将
-- 一个字符串读成 Day 的类型的数据。

-- > read "Mon":: Day
-- Mon

-- 值得一提的是，如果一个数据类型 a 是可读的，另一个 b 类型也可读，那么依赖于 a
-- 的类型 b 也是可读的。比如，数据 Day 是可读的，依赖于 Day 类型的列表 [Day] 也是可读的。

-- > read "[Mon,Tue]" :: [Day]
-- [Mon,Tue]

-- data () = () -- Haskell嵌入的语法，我们无法定义


-- 构造类型

type Name = String
type Author = String
type ISBN = String
type Price = Float

-- data Book = Book Name Author ISBN Price deriving(Show, Eq)

-- data Book 中的 Book 是类型的名字，也称为类型构造器
-- 而 Book Name Author ISBN Float 中的 Book 被称为数据构造器（data constructor）
-- 其实，数据构造器本身是一种特殊的函数，不过首字母会大写。

-- 当给定一个 Book 类型的数据，需要得知书的信息，即访问 Book 构造器的
-- 参数。如此一来，就要写一些重复的、烦人的函数当做访问器。比如：
-- name (Book n _ _ _ ) = n
-- author (Book _ a _ _) = a
-- isbn (Book _ _ i _) = i
-- price (Book _ _ _ p) = p

-- Haskell 语言的设计者们提供了另外一种定义的语法，这个语法使得访问器函数在
-- 这个类型定义的同时也被定义出来。
data Book = Book {
    name :: Name
    , author :: Author
    , isbn :: ISBN
    , price :: Price
} deriving (Eq, Show)

-- 访问器的名称如 name、author 等，被称为字段名或者记录（record）。
increasePrice :: ([Book], [Book]) -> Book -> Float -> ([Book], [Book])

-- increasePrice (b1, b2) b pri =
    -- ((b : b1), Book (name b) (author b) (isbn b) (price b + pri))

-- increasePrice (b1, b2) (Book nm ath isbn prc) pri =
    -- ((Book nm ath isbn prc) : b1, (Book nm ath isbn (prc + pri)) : b2)

-- increatePrice (b1, b2) b@(Book nm ath isbn prc) pri =
    -- (b : b1, (Book nm ath isbn (prc + pri)) : b2)


increasePrice (b1, b2) b pri = (b : b1, (b {price = pri + (price b)}) : b2)
-- Book 这个类型在定义时有 price 这个访问器，所以我们可以
-- 在 b 后面写一个大括号然后把值声明 price=pri 来设置 price 这个参数的值。

-- 有的时候，构造器与值其实并没有显著有区别，比如，
-- 可以认为 True 与 False 就是布尔类型的构造器，它们不需要
-- 参数称为零元数据构造器（nullary data constructor）


-- 参数化类型
data Maybe' a = Nothing' | Just' a

-- 当 a 类型是一个有序的类型，那么 Just 也可以比较大小并且 Nothing
-- 要小于 Just a。而当 a 不是一个有序的类型，Nothing 与 Just 是无法比比较大小的。

safeHead :: [a] -> Maybe' a
safeHead [] = Nothing'
safeHead (x:xs) = Just' x

safeDiv :: Integral a => a -> a -> Maybe' a
safeDiv a 0 = Nothing'
safeDiv a b = Just' (div a b)


-- 类型的类型称为 kind。

-- * 是一个零元类型构造器（nullary type constructor）的 kind，这种类型构造器不需要其
-- 他类型做为参数，自己本身就是一个“完整的”类型。比如，Maybe Bool :: * 与 Maybe Int
-- :: * 都是完整的类型。Haskell 中在 GHCi 里使用:kind（简写为:k）来查询一个类型构造器
-- 的 kind：


data Either' a b = Left' a | Right' b
    deriving (Show, Eq)

-- [Left' 80, Right' "Cheated", Left' 95, Right' "Illness"]

-- 将两个类型可能不同的列表合成一个列表就可以使用这样的函数。
disjoint :: [a] -> [b] -> [Either' a b]
disjoint as bs = map Left' as ++ map Right' bs

-- 当需要把 Either 中的值映射为另一个值时，要为 Left 与 Right 分别提供一个函数，这两
-- 个函数返回的类型相同，在这里都为 c 类型
either' :: (a -> c) -> (b -> c) -> Either' a b -> c
either' f _ (Left' x) = f x
either' _ g (Right' y) = g y

-- 也可以将一个 Either 类型的列表分成两个列表，这个相当于 disjoint 的反函数
partitionEithers :: [Either' a b] -> ([a], [b])
partitionEithers = foldr (either' left right) ([], [])
    where
        left a (l, r) = (a:l, r)
        right a (l, r) = (l, a:r)


-- 由两个组成类型合并为一个类型的 Pair，其实是与 (a,b) 是等价的。这种等价的关系，将在
-- 同构的类型一节中介绍。那么，Haskell 中的二元元组的数据构造器是什么呢？它写作 (,)

-- a -> b 为一个函数的类型，它也是有类型构造器的。它的类型构造器是 (->)，也可以
-- 写做 (->) a b。但它是嵌入在 Haskell 中的，不是直接定义的。它的 kind为 * -> * -> *。


data Pair a b = Pair a b
        deriving (Show, Eq)

pfirst (Pair a b) = a
psecond (Pair a b) = b

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat $ n - 1)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = add m (Succ n)


-- 杂合定义类型

data Shape 
    = Circle {radius :: Float}
    | Rect {len :: Float, width :: Float}
    deriving (Show, Eq)

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect a b) = a * b


-- 当一个构造器有多条记录的时候，在模式匹配时，我们并不需要写出所有的
data Person = Person {
    pname :: String,
    age :: Int,
    sex :: Bool
} deriving (Show, Eq)

showPerson :: Person -> String
showPerson (Person {pname = str, sex = s}) = str ++ show s

data BoolExp = TRUE | FALSE | IF BoolExp BoolExp BoolExp deriving (Show, Eq)

eval :: BoolExp -> Bool
eval TRUE = True
eval FALSE = False
eval (IF cond b1 b2)| eval cond == True = eval b1
                    | eval cond == False = eval b2

-- 参数化递归类型
-- 通常，参数化类型与递归类型是一起使用的。预加载的库中定义了一个非常重要的类型，
-- 就是列表，它就是用了参数化与递归的方式定义的
-- data [] a = [] | a : [a]

data List a = Nil | Cons a (List a) deriving (Show, Eq)
lhead :: List a -> a
lhead Nil = error "empty list"
lhead (Cons a _) = a

mylistToList Nil = []
mylistToList (Cons x xs) = x : (mylistToList xs)


-- 类型的同构
-- 同构（isomorphism），大致
-- 的定义是如果两个类型 A 与 B 之间可以相互转换，也就是说我们可以定义出两个转换函数，
-- 并且这两个函数均为一一对应的函数且互为反函数，则可称类型 A 与 B 是同构的。

-- 同构的类型：对于两个类型 A 与 B，若可以定义 f :: A → B 将 A 映射到 B，
-- 并且可以定义 f 的反函数 g :: B → A 将 B 映射到 A 且满足 f ◦ g = idB 和 g ◦ f = idA（◦
-- 为复合函数运算符，idA 与 idB 分别是 A 与 B 类型上的恒值映射），那么则说类型 A 与类
-- 型 B 为同构的，记做 A ≃ B。

-- 性质 8.2.1. 任意类型都与自己同构，即 A ≃ A。即 ≃ 是自反的（reflexive）。
-- 证明：只需要定义 A → A 的函数，这个函数就是恒值函数 id。
-- 性质 8.2.2. 如果 A 与 B 同构，那么 B 与 A 也同构，即 A ≃ B =⇒ B ≃ A。即 ≃ 是对称
-- 的（symmetric）。
-- 证明：由 A ≃ B 得函数 ψ : A → B 与 ϕ : B → A，根据定义显然 B ≃ A。
-- 性质 8.2.3. 如果 A ≃ B 并且 B ≃ C，那么 A ≃ C，即 A ≃ B ∧ B ≃ C =⇒ A ≃ C。即 ≃
-- 是传递的（transitive）。

data ThreeNum = One | Two | Three
data Level = Low | Middle | High

-- 像上边两个类型那样，所枚举的值的个数是相等的。那么，显然可以写两个函数相互转换，并
-- 且能够满足同构的条件。对于这种情况，可以总结出更为一般的规则：给定两个使用枚举类
-- 型，若它们之中定义的值的个数相等，那么这两个类型是同构的。
f :: ThreeNum -> Level
f One = Low
f Two = Middle
f Three = High

g :: Level -> ThreeNum
g Low = One
g Middle = Two
g High = Three

-- 若一个类型的值有无穷多个，则需要了解基数（cardinal number）的概念来区别不同等
-- 级的无穷才能更为一般地判定两个类型是否同构。

-- 列表是一个参数化定义类型，若给定这个类型参数为 Unit，并且 Unit 类型中仅定义
-- 有一个值——Unit，那么可以看出，List Unit 与 Nat 是同构的。并且我们定义的 Unit 与
-- Haskell 内置的 () 类型是同构的，即：() ≃ Unit

data Unit = Unit
-- data Nat = Zero | Succ Nat
-- data List a = Nil | Cons a (List a)

listToNat Nil = Zero
listToNat (Cons x xs) = Succ (listToNat xs)

natToList Zero = Nil
natToList (Succ n) = Cons Unit (natToList n)


-- 若给定类型 A、B 和 C，那么下列基于它们的元组显然也是同构的：
-- (A, B) ≃ (B, A) ((A, B), C) ≃ (A,(B, C)) ≃ (A, B, C)

-- 函数类型之间与可以是同构的，只是此时对应的转换函数 f 与 g 均为高阶函数。
-- A → (B, C) ≃ A → (C, B)
-- (A, B) → C ≃ A → B → C


-- 元组这样定义的类型，只有一个构造器，构造器内含有多个类型，在书面表达时常常用 × 来表示，如二元元组可定义为 data Pair A B =
-- Pair A B，记作 A × B，称作积类型（product type），在集合中称为笛卡儿积（Cartesian product）。
-- 可以用 |X| 来表示 X 类型的值的个数，那么，则有 |A × B| = |A| × |B|。

-- 而像 Either 类型这样，由多个构造器（或称多模式（multi-pattern））枚举定义的类
-- 型，则用 + 来表示，比如 data Either A B = Left A | Right B，记作 A + B，称作和类
-- 型（sum type），从集合的角度可以理解为不相交并集（disjoint union）。
-- 显然这个类型值的个数是 A 与 B 类型值的个数之和，那么，则有 |A + B| = |A| + |B|。

-- 还有函数类型，如果是 data Fun = Fun (A -> B) 这样的函数类型，记作：B^A，即这样的函数有 |B^|A|| = |B|^|A| 个。

-- 像布尔类型这样以具体值或者称为零元构造器的“和”类型进行计数的方法是一种代数，
-- 如布尔值类型可以写为 1 + 1，第一个 1 表示 False，第二个 1 表示 True，可以看到它与Either () () 类型同构


-- 这样由 () 类型通过和类型、积类型与还有函数类型等方式组合定义出来
-- 的类型称为代数数据类型（algebraic data type，简写为 ADT）。比如，元组、列表、树、四
-- 则运算表达式等，也正是这种定义数据的方式，使得在解决问题时更为灵活。

-- 性质 8.2.4. 柯里化的性质 *：对于任意的函数 f :: (A, B) → C 与 g :: A′ → A 都有：
-- Λ(f) ◦ g = Λ(f ◦ (g × idB))

-- f :: (a, b) -> c
-- f = undefined

-- g :: a' -> a
-- g = undefined

-- (><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)


-- 多分⽀条件、模式匹配守卫、观察模式表达式与模式的别名

-- Haskell 中又引入了另外 3 种表达方式，
-- 分别是多分支的条件，这种表达式的语言扩展是 MultiWayIf。
-- 此外还有模式守卫表达式，对应的语言扩展是 PatternGuards，这种表达方法可以同时做条件限定与模式匹配。
-- 最后还有观察模式表达式，在 GHC 中对应ViewPatterns语言扩展。

foo a1 a2 = if  | a1 > 10 -> if | a1 < 20 && a2 > 50 -> True
                                | a1 >= 10 && a2 < 50 -> False
                | a1 < 10 -> True
                | otherwise -> False

-- foo a1 a2 = if {    | guard1 -> if { | guard2 -> expr2 ;
                    -- | guard3 -> expr3 }
                    -- | guard4 -> expr4 ;
                    -- | guard5 -> expr5 }

-- data Shape = Triangle Int Int Int | Circle Int
-- isValidShape :: Shape -> Bool
-- isValidShape s | Circle r <- s, r > 0 = True
-- isValidShape s | Triangle a b c <- s,a > 0 && b > 0 && c > 0, a + b > c && a + c > b && b + c > a = True
-- isValidShape _ = False


match' :: Seq Int -> Seq Int -> (Int, Seq Int)
match' (viewl -> EmptyL) s2@(viewr -> EmptyR) = (0,s2)
match' (viewl -> EmptyL) (viewr -> xs :> x) = (x,xs)
match' (viewl -> a :< as) (viewr -> EmptyR) = (a,as)
match' (viewl -> a :< as) (viewr -> xs :> x) = (a + x, xs >< as)

-- 观察模式匹配中-> 左侧的是观察函数，它是一个需要一个参数得到结果，我会需要把模式写在箭头的右侧。


-- 模式的别名

-- 有时，类型的定义会比较复杂，对于这些类型在匹配的时候代码会十分不干净，而且更重要的是我们在定义函数时“模式”是无法复用的

data Exp = Val Int | Exp String [Exp]
    deriving  (Show, Eq)

pattern Add t1 t2 = Exp "+" [t1, t2]
pattern Sub t1 t2 = Exp "-" [t1, t2]

-- let a = Add (Val 1) (Val 2)

eval' (Val n) = n
eval' (Add t1 t2) = eval' t1 + eval' t2
eval' (Sub t1 t2) = eval' t1 - eval' t2
-- 等价于
eval'' (Val n) = n
eval'' (Exp "+" [t1, t2]) = eval'' t1 + eval'' t2
eval'' (Exp "-" [t1, t2]) = eval'' t1 - eval'' t2


-- 使⽤ newtype 定义类型

-- newtype 只能定义单一构造器，并且该构造器只能有且仅有一个参数。
-- data 关键字是可以完全代替 newtype 的，但反之则可能不成立。

-- 下面的定义是合法的，因为 (a,b) 将作为 NewType 构造器的唯一一个参数
-- newtype T a b = NewType (a, b)
-- newtype 定义的类型是一个新的类型。并且 newtype 关键字也可以递归地定义一个类型。
-- 另外，使用 newtype 定义一个类型可以理解为是将已有的某些类型整合为一个类型，只是在这个类型外多了一个标记（tag）而已。

-- 使用 data 定义类型会在编译系统类型检查与运行中产生额外的运行负担，而有的时候我们只是为了给某个已有的类型加一层包装来表示区分

newtype Velocity = Velocity Int deriving (Num, Show ,Eq)
newtype Weight = Weight Int deriving (Num, Show, Eq)
newtype Second = Second Int deriving (Num, Show, Eq)


-- 树
data Tree a = Leaf a | Node a (Tree a) (Tree a)
data Tree' a = Node' a [Tree' a]
-- 上面的定义称为 rose tree，可以有很多叉或者没有叉。它为什么不需要定义基本形式 Leaf呢？因为当后跟的列表为空的时候就可以理解为一个基本形式了。
-- 这种方式也是 Haskell 库中树的定义
-- Data.Tree 中源文件中是这样定义的：
-- data Tree a = Node
-- { rootLabel :: a
-- , subForest :: Forest a
-- }

-- type Forest a = [Tree a]

-- Haskell 中的树与其他语言里的树有很大差别的，纯函数中不能定义 C 语言中的指针变量。
-- Haskell 定义树的各个结点间是不会形成环的，而 Java 或者 C 语言语言中定义的树是通
-- 过对象引用或者指针变量定义。这样，一棵子树可能被树中多处引用。
-- parent.left = & child ;
-- parent.right = & child ;

-- 卡塔兰数问题

-- 给定 n 个结点，一共能组成多少种二叉树，所得到的数列就是卡塔兰数。

-- 思路就是将一个节点取出作为父节点，然后假定余下的节点个数为 n，将 n 拆分成不
-- 同整数的和，可以得到列表 [(0,n), (1,n-1),(2,n-2)...]，然后对于一个整数对 (l,r) 递
-- 归地用这个函数将树生成出左树与右树后加在父节点的左右就可以了。

data CTree = CLeaf | CNode CTree CTree deriving (Show)
treeNum :: Int -> [CTree]
treeNum 0 = [CLeaf]
treeNum n = [CNode lt rt | l <- [0..(n-1)], lt <- treeNum l, rt <- treeNum (n-1-l)]


-- 霍夫曼编码

-- [("p1",0.4),("p2",0.3), ("p3",0.1),("p4",0.1),("p5",0 .06),("p6",0.04)]

data HTree a = HLeaf a | Branch (HTree a) (HTree a) deriving Show

htree :: (Ord a, Num a) => [(a, HTree b)] -> HTree b
htree [(_, t)] = t
htree ((w1, t1): (w2, t2):xs) = htree $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) xs

serialize :: HTree a -> [(a, String)]
serialize (HLeaf x) = [(x, "")]
serialize (Branch l r) = 
    [(x, '0':code) | (x, code) <- serialize l] ++ [(x, '1':code) | (x, code) <- serialize r]
    
huffman :: (Ord a, Ord w, Num w) => [(a, w)] -> [(a, String)]
huffman freq = Data.List.sortBy (comparing fst) $ -- 按名字排序
        serialize $ htree $ Data.List.sortBy (comparing fst) $ [(w, HLeaf x) | (x, w) <- freq] -- 从右到左 sort build-tree traverse


-- Zipper

-- 当我们需要在一个容器中反复多次游历时需要在游历的过程中保持容器结构与其中值的完整性。
-- 当我们需要在列表中来回游历时就需要使用拉锁（Zipper）类型。

data Zipper a = Zipper [a] a [a] deriving Show

fromList' :: [a] -> Zipper a
fromList' (x:xs) = Zipper [] x xs
fromList' _ = error "empty list"

-- 当前所在的位置为 a，向后移动一次就将 Zipper 当前的元素暂时存入左面的列表中，再从右端列表中取出一个元素作为当前位置。再向前移动也是类似的。
next :: Zipper a -> Zipper a
next (Zipper xs a (y:ys)) = Zipper (a:xs) y ys
next z = z

prev :: Zipper a -> Zipper a
prev (Zipper (x:xs) a ys) = Zipper xs a (x:ys)
prev z = z




