-- 在编写 Haskell 代码时，第一行可以声明一些编译器参数（如果不需要编译器参数，那么
-- 这一行可以省略）。然后，以 module XXX where 的格式定义该模块的名字。通过这种方法，
-- 可以控制其他模块对该模块中的函数与类型的访问。最后，定义该程序文件下的函数、类型等。

module Test (f1, f2) where
-- 如果不在 Test 后写括号，则默认所有的函数都是对外可见的。如果不想把该代码文件当做模块在别处调用，那么可以在 Test 后面加空括号() 即可。或者直接不给出 module 的定义。

f1 = (+)
f2 = (-)

-- private f3
f3 = (*)


-- import Test (f1)
-- 这样，只有 f1 被导入到当前的代码中。

-- 一些库函数与用户定义的函数可能是有命名冲突的（或者不想使用），这时可以使用 hiding 关键字将它们的定义隐藏
-- import Prelude hiding (catch)

-- 有时，需要导入多个模块，但是其中的两个模块下有两个函数名称一样，可是又需要同
-- 时使用它们。这时，可以使用 qualified 关键字来对不同的模块命名
-- import qualified Test as T


-- Boolean
(==) :: Bool -> Bool -> Bool
(==) True True = True
(==) False False = True
(==) _ _ = False

not :: Bool -> Bool
not True = False
not _ = True

xor, and, or :: Bool -> Bool -> Bool
xor b1 b2 = not (b1 == b2)

and True b1 = b1
and False _ = False

or False b1 = b1
or True _ = True

{-
and b1 b2 = if b1 then b2 else False
or b1 b2 = if b1 then True else b2
-}

condition :: Bool -> a -> a -> a
condition True t f = t
condition False t f = f

-- 现在试着用运算符来代替它们
infix 4 ==
infix 4 /=
infix 3 &&
infix 2 ||

(||) = or
(&&) = and
(/=) = xor

-- 在逻辑门电路中，有两个非常重要的逻辑门——nand（与非门）与 nor（或非门），即
-- not and 与 not or。这两个逻辑门称为“通用逻辑门”（universal logic gate），它们的特殊在
-- 之处在于仅仅用 nand 与 nor 中的一个就可以定义出其他所有的逻辑门。
nand, nor :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

nor False False = True
nor _ _ = False

not1, not2 :: Bool -> Bool
not1 b = nand b b
not2 b = nor b b

and1, and2 :: Bool -> Bool -> Bools
and1 b1 b2 = nand (nand b1 b2) (nand b1 b2)
and2 b1 b2 = nor (nor b1 b1) (nor b2 b2)

or1, or2 :: Bool -> Bool -> Bool
or1 b1 b2 = nand (nand b1 b1) (nand b2 b2)
or2 b1 b2 = nor (nor b1 b2) (nor b1 b2)