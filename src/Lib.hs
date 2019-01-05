--
-- 標準ライブラリの関数を書き直したモジュール
--
module Lib where

import Data.List

-- | 否定
--
-- >>> not True
-- False
-- >>> not False
-- True
--
not' :: Bool -> Bool
not' True  = False
not' False = True

-- | リストに要素が含まれているか判定（`elem`の独自実装）
--
-- >>> elem' 1 []
-- False
-- >>> elem' 1 [1, 2]
-- True
-- >>> elem' 3 [1, 2]
-- False
--
-- 標準と比較して検証：
-- prop> elem' x xs == elem x xs
--
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

-- | `even`の独自実装
--
-- >>> even' 1
-- False
-- >>> even' 2
-- True
--
-- 標準と比較して検証：
-- prop> even' x == even x
--
even' :: (Integral a) => a -> Bool
even' a = a `mod` 2 == 0

-- | `length`の独自実装
--
-- >>> length' []
-- 0
-- >>> length' [1]
-- 1
-- >>> length' [1,2,3]
-- 3
--
-- 標準と比較して検証：
-- prop> length' x == length x
--
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- | `init`の独自実装
--
-- 標準APIと比較：
-- prop> xs /= [] ==> init' xs == init xs
--
init' :: [a] -> [a]
init' []     = errorWithoutStackTrace "list is empty"
init' [x]    = []
init' (x:xs) = x : init xs

-- | `last`の独自実装
--
-- 標準APIと比較：
-- prop> xs /= [] ==> last' xs == last xs
--
last' :: [a] -> a
last' []     = errorWithoutStackTrace "list is empty"
last' [x]    = x
last' (_:xs) = last' xs

-- | `filter`の独自実装
--
-- >>> filter' (\_ -> True) []
-- []
-- >>> filter' (\x -> x == 1) [1]
-- [1]
-- >>> filter' (\x -> x == 1) [1, 2]
-- [1]
-- >>> filter' (\x -> x == 1) [2, 1]
-- [1]
-- >>> filter' (\x -> x == 1) [1, 1, 2]
-- [1,1]
--
-- 標準と比較して検証：
-- prop> filter' (\x -> even' x) xs == filter (\x -> even' x) xs
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x
    then x : ys
    else ys
        where ys = filter' f xs

-- | リストのソート（`sort`の独自実装）
--
-- >>> sort' []
-- []
-- >>> sort' [1]
-- [1]
-- >>> sort' [2, 1]
-- [1,2]
-- >>> sort' [1, 1]
-- [1,1]
-- >>> sort' [3, 2, 1]
-- [1,2,3]
-- >>> sort' [3, 3, 1]
-- [1,3,3]
-- >>> sort' [3, 1, 1]
-- [1,1,3]
-- >>> sort' [1, 1, 1]
-- [1,1,1]
-- >>> sort' [3, 2, 4, 1]
-- [1,2,3,4]
--
-- 標準と比較して検証：
-- prop> sort' xs == sort xs
--
sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x:xs) = ys ++ [x] ++ zs
    where
        ys = sort' $ filter' (\y -> y <= x) xs
        zs = sort' $ filter' (\z -> z > x) xs

-- | `reverse`の独自実装
--
-- >>> reverse' []
-- []
-- >>> reverse' [1]
-- [1]
-- >>> reverse' [1,2,3]
-- [3,2,1]
--
-- 標準と比較して検証：
-- prop> reverse' xs == reverse xs
--
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- | `takeWhile`の独自実装
--
-- >>> takeWhile (< 3) []
-- []
-- >>> takeWhile (< 3) [1..4]
-- [1,2]
--
-- 標準と比較して検証：
-- prop> takeWhile' (< 10) xs == takeWhile (< 10) xs
--
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x
    then x : takeWhile' f xs
    else []


-- | `dropWhile`の独自実装
--
-- >>> dropWhile (< 3) []
-- []
-- >>> dropWhile (< 3) [1]
-- []
-- >>> dropWhile (< 3) [1..4]
-- [3,4]
--
-- 標準と比較して検証：
-- prop> dropWhile' (< 10) xs == dropWhile (< 10) xs
--
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f zs@(x:xs) = if f x
    then dropWhile' f xs
    else zs

-- | `span`の独自実装
--
-- >>> span' (1 ==) []
-- ([],[])
-- >>> span' (1 ==) [1, 1, 1, 2, 3]
-- ([1,1,1],[2,3])
--
-- 標準と比較して検証：
-- prop> span' (< 3) xs == span (<3) xs
--
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' f xs = (takeWhile' f xs, dropWhile' f xs)

-- | `group`の独自実装
--
-- >>> group' [1]
-- [[1]]
-- >>> group' [1, 1]
-- [[1,1]]
-- >>> group' [1, 1, 2]
-- [[1,1],[2]]
-- >>> group' [1, 2, 2]
-- [[1],[2,2]]
-- >>> group' [1, 1, 1]
-- [[1,1,1]]
--
-- 標準と比較して検証：
-- prop> group' xs == group xs
--
group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = (x:ys) : group' zs
    where
        (ys, zs) = span' (x ==) xs
