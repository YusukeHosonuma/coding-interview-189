module Q3 where

import Lib

-- | 循環リストの先頭を探す
--
-- >>> findCycleStart $ zip ([0..4] ++ cycle [5..9])  [0..]
-- 5
-- >>> findCycleStart $ zip ([0..4] ++ cycle [5..10]) [0..]
-- 5
-- >>> findCycleStart $ zip ([0..5] ++ cycle [6..10]) [0..]
-- 6
-- >>> findCycleStart $ zip ([0..5] ++ cycle [6..11]) [0..]
-- 6
-- >>> findCycleStart $ zip ([0] ++ cycle [1..2]) [0..]
-- 1
-- >>> findCycleStart $ zip ([0] ++ cycle [1]) [0..]
-- 1
--
-- prop> x >= 1 && y >= 1 ==> findCycleStart (zip (take x [0..] ++ (cycle (take y [x..]))) [0..]) == x
--
findCycleStart :: [(Int, a)] -> a
findCycleStart []  = errorWithoutStackTrace "list is empty"
findCycleStart [x] = errorWithoutStackTrace "list is only one element"
findCycleStart xs  = snd . fst $ head $ dropWhile (\((x, _), y) -> x /= y) $ zip xs ys
    where
        ys = map snd $ dropWhile notEq $ tail zs
        zs = zip (foldr (\x acc -> x : -1 : acc) [] as) as
        as = map fst xs
        notEq (x, y) = x /= y
