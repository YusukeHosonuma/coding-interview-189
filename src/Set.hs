module Set (
    empty',
    size',
    insert'
) where

-- | `Set`の独自実装
newtype Set' a = Set' [a] deriving (Show)

-- | 空のセットを返す
--
-- >>> empty' :: Set' Int
-- Set' []
--
empty' :: Set' a
empty' = Set' []

-- | セットのサイズを返す
--
-- >>> size' (empty' :: Set' Int)
-- 0
-- >>> size' $ insert' 1 empty'
-- 1
-- >>> size' $ insert' 2 $ insert' 1 empty'
-- 2
--
size' :: Set' a -> Int
size' (Set' xs) = length xs

-- | セットに要素を追加する
--
-- >>> insert' 1 empty'
-- Set' [1]
-- >>> insert' 2 $ insert' 1 empty'
-- Set' [2,1]
-- >>> insert' 1 $ insert' 1 empty'
-- Set' [1]
--
insert' :: (Eq a) => a -> Set' a -> Set' a
insert' x ys@(Set' xs)
    | elem x xs = ys
    | otherwise = Set' (x:xs)
