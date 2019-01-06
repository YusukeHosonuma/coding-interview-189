module Q3 where

import Lib

-- | 循環リストの先頭を探す
--
-- >>> findCycleStart $ [(x, x) | x <- [0..4] ++ cycle [5..9]]
-- 5
-- >>> findCycleStart $ [(x, x) | x <- [0..4] ++ cycle [5..10]]
-- 5
-- >>> findCycleStart $ [(x, x) | x <- [0..5] ++ cycle [6..10]]
-- 6
-- >>> findCycleStart $ [(x, x) | x <- [0..5] ++ cycle [6..11]]
-- 6
-- >>> findCycleStart $ [(x, x) | x <- [0] ++ cycle [1..2]]
-- 1
-- >>> findCycleStart $ [(x, x) | x <- [0] ++ repeat 1]
-- 1
--
-- prop> x >= 1 && y >= 1 ==> findCycleStart [(x, x) | x <- take x [0..] ++ cycle (take y [x..])] == x
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
