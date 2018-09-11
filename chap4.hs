
-- 常⽤函数
-- 恒值函数 id :: a -> a，它是给定一个任何的值，都返回这个给定的值的函数。
-- id :: a -> a

-- 常值函数 const 函数 const :: a -> b -> a 的作用是给定两个元素，只返回第一个
-- const :: a -> b -> a

-- > const id True 3
-- 3

-- const id True 3
-- = (const id True) 3 {函数应用为左结合}
-- = id 3 {应用 const 函数，将 id 函数返回}
-- = 3 {应用 id 函数}

-- 参数反置函数 flip
-- 函数 flip :: (a -> b -> c) -> b -> a -> c 可以将一个二元函数的两个参数的顺序颠倒
-- flip (-) 3 8

-- 错误函数 error
-- 异常函数 error :: String -> a 是抛出异常的函数。有些时候，程序中出现的错误会
-- 导致整个程序停止运行。这时，就可以用 error 函数来返回错误信息。返回时，使用
-- 一个字符串来告诉用户为什么有错误。
-- error :: String -> a

-- undefined
-- 未定义 undefined 函数是由 error 函数定义的。
-- 由于 undefined 是一个多态类型 a，因此可以用其暂时定义没有实现的函数，这是为
-- 了保证调试时 GHCi 不会报错。
-- undefined :: a
-- undefined = error "Prelude; undefined"


-- min 与 max
-- 这是一对常用的函数，min :: Ord a => a -> a -> a 将返回两个参数中较小的那一个，
-- 而 max 则会返回两个参数中较大的那一个。

-- null 函数会判定一个列表是否为空
-- null :: Foldable t => t a -> Bool

-- length 函数会返回列表的长度
-- length :: Foldable t => t a -> Bool

-- (!!)
-- 这个运算符可以取得给定列表中从 0 开始的第 n 个元素。
-- (!!) :: [a] -> Int -> a

-- reverse 倒置函数可以将列表中元素的顺序倒过来
-- reverse :: [a] -> [a]

-- head 和 last
-- 这两个函数分别取一个列表的第一个元素与最后一个元素。它们的类型都是 [a] -> a。
-- 如果列表为空，则会报出错误。
-- head, last :: [a] -> a

-- init 和 tail
-- 它们分别将一个列表的最后一个元素与第一个元素去掉，得到一个新的列表。它们的类
-- 型是 [a] -> [a]。如果列表为空，则会报出错误。
-- init, tail :: [a] -> [a]

-- map
-- map 意为映射，会将一个函数应用到列表中的每一个元素，然后得一个新的列表。
-- map :: (a -> b) -> [a] -> [b]。

-- filter 是过滤函数，需要一个条件判断的函数为参数，使得可以从一个列表中选出满足
-- 给定条件的元素。
-- filter :: (a -> Bool) -> [a] -> [a]

-- even 与 odd
-- 给定一个整数，通过这两个函数可以判断其奇偶性
-- even, odd :: Integral a => a -> Bool

-- take 和 drop
-- take 函数可以从头连续地取得一个列表的几个元素。
-- take, drop :: Int -> [a] -> [a]

-- span 和 break
-- 这两个函数的类型都是 (a -> Bool) -> [a] -> ([a], [a])。span 函数可以根据一个
-- 条件，从左至右，当遇到第一个不符合条件的元素时停止，将一个列表分成由两个列表
-- 组成的元组。break 函数则与 span 函数相反，它会根据一个条件，从左至右，当遇到符合条件的时候停止。
-- span, break :: (a -> Bool) -> [a] -> ([a], [a])

-- takeWhile 和 dropWhile
-- 之前的 take 和 drop 函数是通过给定一个整数来取得或者去掉列表中的前几个元素，
-- 而 takeWhile 和 dropWhile 则需要一个条件来判断，条件不成立的时候停止取出或者
-- 去除。实质上 takeWhile 取的是 span 结果的第一个列表，dropWhile 取的是 span 结
-- 果的第二个列表。
-- takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

-- splitAt
-- 这个函数可以将一个列表在任何的位置分开。
-- splitAt :: Int -> [a] -> ([a], [a])

-- splitAt' :: Int -> [a] -> ([a], [a])
-- splitAt' n xs = (take n xs, drop n xs)

-- repeat 和 replicate
-- 重复函数 repeat 可以将一个元素在列表里重复无数次
-- repeat :: a -> [a]
-- replicate :: Int -> a -> [a]
-- replicate' :: Int -> a -> [a]
-- replicate' n a = take n (repeat a)

-- any 和 all
-- any 可以查询一个列表中是否存在符合给定条件的元素，
-- 而 all 会判定列表中是否所有的元素都符合给定条件。
-- any :: Foldable t => (a -> Bool) -> t a -> Bool
-- all :: Foldable t => (a -> Bool) -> t a -> Bool

-- and 和 or
-- and 会把一个列表中所有的布尔值用 && 连接起来，如果是空列表那么返回 True；
-- 而 or 则会把所有布尔值用 || 连接起来，如果列表为空则返回 False。
-- and :: Foldable t => t Bool -> Bool
-- or :: Foldable t => t Bool -> Bool

-- elem 和 notElem
-- elem 函数可以判断一个列表中是否存在某一元素。显然，a 一定要是可以比较相等的类型，
-- 所以在类型签名中需要 Eq 类型类。
-- 同样，prelude 中还有 notElem，notElem 是 elem 的否定。
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem' :: Eq a => a -> [a] -> Bool
-- elem' a xs = not(null (filter (==a) xs))

-- iterate
-- 它可以将第一个参数中的函数应用在第二个参数上，并重复应用到函数结果上。
-- iterate :: (a -> a) -> a -> [a]

-- until
-- until 可以迭代地来生成数据直到满足给定的条件为止。
-- 它需要一个停止条件、一个迭代的函数还有一个迭代的初始值，首次到达条件时停止，然后返回结果。
-- until :: (a -> Bool) -> (a -> a) -> a -> a

-- > until (>500) (*2) 1
-- 512


-- zip 
-- zip 函数可以将两个列表结合成一个元组的列表。当元素个数不相等的时候，多余的元素会被忽略。
-- zip :: [a] -> [b] -> [(a, b)]

-- unzip
-- unzip 是把一个二元元素列表分成两个列表元素的函数，即将 zip 后的列表还原，
-- unzip :: [(a, b)] -> ([a],[b])

-- zipWith
-- 它需要一个二元函数作为参数，然后再输入两个列表，
-- 最后把两个列表中的元素取出一一对应地进行第一个参数指定的二元运算。
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- 当然，有的时候要用到将三个列表的中元素合成有三个元件的元组，这时可以用到预加
-- 载库中的 zip3、unzip3。此外，Prelude 中还提供 zipWith3 等函数

-- concat
-- concat 函数可以将一个列表中的列表相连
-- concat :: Foldable t => t [a] -> [a]

-- > concat [[1,2],[3,4]]
-- [1,2,3,4]

-- concatMap
-- 这个函数先使用 map 函数将 [a] 计算为 [[b]] 类型的结果，
-- 再使用 concat 函数来得到类型为 [b] 的结果。
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]

-- map (replicate 3) [1,2,3]
-- [[1,1,1],[2,2,2],[3,3,3]]

-- > concatMap (replicate 3) [1,2,3]
-- [1,1,1,2,2,2,3,3,3]