-- ある文字列が、全てユニークである（重複する文字がない）かどうかを判定するアルゴリズムを実装してください。
-- また、それを実装するのに新たなデータ構造が使えない場合、どのようにすればよいですか？
module Q1 where

import Lib
import Set

-- | 文字がすべて異なるか判定する。（再帰バージョン）
--
-- >>> isUnique ""
-- True
-- >>> isUnique "a"
-- True
-- >>> isUnique "ab"
-- True
-- >>> isUnique "aa"
-- False
-- >>> isUnique "abc"
-- True
-- >>> isUnique "aac"
-- False
-- >>> isUnique "acc"
-- False
-- >>> isUnique "aba"
-- False
--
-- ユニークな文字列は、ソート・グループした文字数の長さと、元の文字列の長さが一致する
-- prop> isUnique s == ((length' $ group' $ sort' s) == length' s)
--
isUnique :: String -> Bool
isUnique [] = True
isUnique [_] = True
isUnique (x:xs) = not' (x `elem'` xs) && isUnique xs

-- | 畳み込みバージョン
--
-- 再帰バージョンと比較：
-- prop> isUnique s == isUnique' s
--
isUnique' :: String -> Bool
isUnique' xs = length' xs == size' ys
    where ys = foldr insert' Set.empty' xs
