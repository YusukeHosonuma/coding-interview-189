--
-- 連結リストが回文(先頭から巡回しても末尾から巡回しても、
-- 各ノードの要素がまったく同じになっている)かどうかを調べる関数を実装してください。
--
module Q2 where

import Lib

-- | 回文かどうか判定する
--
-- >>> isPalindrome "たけやぶやけた"
-- True
-- >>> isPalindrome "こんにちは"
-- False
-- >>> isPalindrome ""
-- True
-- >>> isPalindrome "あ"
-- True
--
isPalindrome xs = xs == reverse' xs

-- | 回文かどうか判定する
--
-- >>> isPalindrome' "たけやぶやけた"
-- True
-- >>> isPalindrome' "こんにちは"
-- False
-- >>> isPalindrome' ""
-- True
-- >>> isPalindrome' "あ"
-- True
--
-- 明白な実装である`isPalindrome`と比較：
-- prop> isPalindrome xs == isPalindrome' xs
--
isPalindrome' []       = True
isPalindrome' [x]      = True
isPalindrome' [x, y]   = x == y
isPalindrome' (x:y:xs) = x == last' xs && isPalindrome' (y : init' xs)
