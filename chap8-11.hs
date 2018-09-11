{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}


-- 8.11 ⼀般化的代数数据类型

-- data Exp a = 
--     ValInt Int
--     | ValBool Bool
--     | Add (Exp a) (Exp a)
--     | Equa (Exp a) (Exp a)
--     deriving (Eq, Show)

-- eval :: Exp a -> Either Int Bool
-- eval (ValInt i) = i
-- eval (ValBool b) = Right b
-- eval (Add e1 e2) =
--     case eval e1 of 
--         Left a ->
--             case eval e2 ->
--                 Left b -> Left (a + b)

-- eval (Equa e1 e2) = 
--     case eval e1 of 
--         Left a ->
--             case eval e2 ->
--                 Left b -> Left (a == b)


data Exp a where
    ValInt :: Int -> Exp Int
    ValBool :: Bool -> Exp Bool
    Add :: Exp Int -> Exp Int -> Exp Int
    Equa :: Exp Int -> Exp Int -> Exp Bool

-- 这里的类型 a 称为虚幻类型（phantom type），一个有着 Exp a 类型的值不会包括一个有着a 类型的值

-- Haskell 中提供的一般化的数据类型可以让构造器携带更多的类型信息来对类型做出需要
-- 的限制。但是，需要在文件首处加入语言扩展 {-# LANGUAGE GADTs #-}

-- 使用 GADT 的代价之一就是不能通过简单地 deriving 来导出Eq、Show 等类型类。

eval :: Exp a -> a
eval (ValInt i) = i
eval (ValBool b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Equa e1 e2) = eval e1 == eval e2

-- 如此一来，一些不合法的表达式就会被类型系统检查出来，不会通过编译从而不会得到
-- 因未定义某些匹配而导致运行时错误，但同时也付出了另一些代价。这个代价就是 eval 函
-- 数类型签名中的 a 类型所指的并不是所有类型。这里仅仅是指 Bool 与 Int 类型，所以这个
-- 类型签名不能准确地表达出 eval 函数的类型。

-- 使用 GADT 定义数据类型和使用常规方法定义数据类型，其区别在于：在 GADT 里，
-- 需要明确指出每个构造器的类型，而在常规方法里，每个构造器的类型由编译器自动生成而且严格受限。



-- 8.11.1 简易谓词逻辑计算器
data Formula ts where
    Body    :: Term Bool -> Formula ()
    Forall  :: Show a => [a] -> (Term a -> Formula as) -> Formula (a, as)
    Exist   :: Show a => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
    Con :: t -> Term t
    :&: :: Term Bool -> Term Bool -> Term Bool
    :|: :: Term Bool -> Term Bool -> Term Bool
    :<: :: Term Int -> Term Int -> Term Bool
    :=: :: Term Int -> Term Int -> Term Bool
    :+: :: Term Int -> Term Int -> Term Int
    :-: :: Term Int -> Term Int -> Term Int
    Name :: String -> Term t

ex1 :: Formula ()
ex1 = Body(Con True)

ex2 :: Formula(Int, ())
ex2 = Forall [1 .. 10] $ \n -> Body $ n :<: (n :+: Con 1)

-- ex3 :: Formula (Bool, (Int, ()))
-- ex3 = Forall [False, True] $ \p -> Forall [0 .. 2] $ \n -> Body $ p :|: (Con 0 :<: n)