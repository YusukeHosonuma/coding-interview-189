module S1 where

import Data.List (nub, sort, group, groupBy, transpose, (\\))
import Data.Function

type Cell  = (Int, Int)
type Board = [(Cell, Bool)]

-- | リストがユニークか判定
isUnique :: (Eq a) => [a] -> Bool
isUnique []     = True
isUnique [_]    = True
isUnique (x:xs) = (x `notElem` xs) && isUnique xs

-- | ルールを満たしているか
--
valid :: Int -> [Cell] -> Bool
valid n cells = validX cells && validY cells && validZ cells n

-- | 横方向のルールを満たしているか
--
validX :: [Cell] -> Bool
validX cells = isUnique $ map fst cells

-- | 縦方向のルールを満たしているか
--
validY :: [Cell] -> Bool
validY cells = isUnique $ map snd cells

-- | 斜め方向のルールを満たしているか
--
validZ :: [Cell] -> Int -> Bool
validZ cells n = not $ any (\px -> length px - 2 >= length (px \\ cells)) $ zPatterns n

-- | 斜めの配置パターン
--
zPatterns :: Int -> [[Cell]]
zPatterns n = zPattern1 n ++ zPattern2 n

-- | 斜め「\」の配置パターン
--
-- >>> length $ zPattern1 4
-- 5
-- >>> length $ zPattern1 5
-- 7
-- >>> length $ zPattern1 6
-- 9
-- >>> length $ zPattern1 7
-- 11
--
zPattern1 :: Int -> [[Cell]]
zPattern1 n = map (`shift` base) [(negate n + 2)..(n - 2)]
    where
        base    = [ (x, x) | x <- [0..(n - 1)] ]
        shift z = filter (inBound n) . map (\(x, y) -> (x + z, y))

-- | 斜め「/」の配置パターン
--
-- >>> length $ zPattern2 4
-- 5
-- >>> length $ zPattern2 5
-- 7
-- >>> length $ zPattern2 6
-- 9
-- >>> length $ zPattern2 7
-- 11
--
zPattern2 :: Int -> [[Cell]]
zPattern2 n = map (`shift` base) [(negate n + 2)..(n - 2)]
    where
        base    = [ (x, (n - 1) - x) | x <- [0..(n - 1)] ]
        shift z = filter (inBound n) . map (\(x, y) -> (x + z, y))

inBound :: Int -> Cell -> Bool
inBound n (x, y) = inRange x && inRange y
    where
        inRange x = 0 <= x && x < n

-- | 4クイーン
--
-- >>> length queen4
-- 2
--
queen4 :: [[Cell]]
queen4 = nub [ sort [a, b, c, d]
             | a <- xs
             , b <- [ x | x <- xs, x /= a,                valid size [a, x] ]
             , c <- [ x | x <- xs, x `notElem` [a, b],    valid size [a, b, x] ]
             , d <- [ x | x <- xs, x `notElem` [a, b, c], valid size [a, b, c, x] ]
             ]
        where
            xs  = patterns' size
            size = 4

-- | 5クイーン
--
-- >>> length queen5
-- 10
--
queen5 :: [[Cell]]
queen5 = nub [ sort [a, b, c, d, e]
             | a <- xs
             , b <- [ x | x <- xs, x /= a,                   valid size [a, x] ]
             , c <- [ x | x <- xs, x `notElem` [a, b],       valid size [a, b, x] ]
             , d <- [ x | x <- xs, x `notElem` [a, b, c],    valid size [a, b, c, x] ]
             , e <- [ x | x <- xs, x `notElem` [a, b ,c, d], valid size [a, b, c, d, x] ]
             ]
        where
            xs  = patterns' size
            size = 5

-- | 6クイーン
--
-- >>> length queen6
-- 4
--
queen6 :: [[Cell]]
queen6 = nub [ sort [a, b, c, d, e, f]
             | a <- ptns (0, 0)
             , b <- [ x | x <- ptns a, x /= a,                      valid size [a, x] ]
             , c <- [ x | x <- ptns b, x `notElem` [a, b],          valid size [a, b, x] ]
             , d <- [ x | x <- ptns c, x `notElem` [a, b, c],       valid size [a, b, c, x] ]
             , e <- [ x | x <- ptns d, x `notElem` [a, b ,c, d],    valid size [a, b, c, d, x] ]
             , f <- [ x | x <- ptns e, x `notElem` [a, b ,c, d, e], valid size [a, b, c, d, e, x] ]
             ]
        where
            ptns = patterns2' size
            size = 6

-- | 7クイーン
--
-- >>> length queen7
-- 40
--
queen7 :: [[Cell]]
queen7 = nub [ sort [a, b, c, d, e, f, g]
             | a <- ptns (0, 0)
             , b <- [ x | x <- ptns a, x /= a,                         valid size [a, x] ]
             , c <- [ x | x <- ptns b, x `notElem` [a, b],             valid size [a, b, x] ]
             , d <- [ x | x <- ptns c, x `notElem` [a, b, c],          valid size [a, b, c, x] ]
             , e <- [ x | x <- ptns d, x `notElem` [a, b ,c, d],       valid size [a, b, c, d, x] ]
             , f <- [ x | x <- ptns e, x `notElem` [a, b ,c, d, e],    valid size [a, b, c, d, e, x] ]
             , g <- [ x | x <- ptns f, x `notElem` [a, b ,c, d, e, f], valid size [a, b, c, d, e, f, x] ]
             ]
        where
            ptns = patterns2' size
            size = 7


-- | 8クイーン
--
-- |>>> length queen8
-- 92
--
queen8 :: [[Cell]]
queen8 = nub [ sort [a, b, c, d, e, f, g, h]
             | a <- ptns (0, 0)
             , b <- [ x | x <- ptns a, valid size [a, x] ]
             , c <- [ x | x <- ptns b, valid size [a, b, x] ]
             , d <- [ x | x <- ptns c, valid size [a, b, c, x] ]
             , e <- [ x | x <- ptns d, valid size [a, b, c, d, x] ]
             , f <- [ x | x <- ptns e, valid size [a, b, c, d, e, x] ]
             , g <- [ x | x <- ptns f, valid size [a, b, c, d, e, f, x] ]
             , h <- [ x | x <- ptns g, valid size [a, b, c, d, e, f, g, x] ]
             ]
        where
            ptns = patterns2' size
            size = 8

-- | 9クイーン
--
-- |>>> length queen8
-- 352
--
queen9 :: [[Cell]]
queen9 = nub [ sort [a, b, c, d, e, f, g, h, i]
             | a <- ptns (0, 0)
             , b <- [ x | x <- ptns a, x /= a,                               valid size [a, x] ]
             , c <- [ x | x <- ptns b, x `notElem` [a, b],                   valid size [a, b, x] ]
             , d <- [ x | x <- ptns c, x `notElem` [a, b, c],                valid size [a, b, c, x] ]
             , e <- [ x | x <- ptns d, x `notElem` [a, b ,c, d],             valid size [a, b, c, d, x] ]
             , f <- [ x | x <- ptns e, x `notElem` [a, b ,c, d, e],          valid size [a, b, c, d, e, x] ]
             , g <- [ x | x <- ptns f, x `notElem` [a, b ,c, d, e, f],       valid size [a, b, c, d, e, f, x] ]
             , h <- [ x | x <- ptns g, x `notElem` [a, b ,c, d, e, f, g],    valid size [a, b, c, d, e, f, g, x] ]
             , i <- [ x | x <- ptns g, x `notElem` [a, b ,c, d, e, f, g, h], valid size [a, b, c, d, e, f, g, h, x] ]
             ]
        where
            ptns = patterns2' size
            size = 9

-- | 1駒を置くときのパターン
--
patterns' :: Int -> [Cell]
patterns' size = [ (x, y) | x <- [0..(size-1)], y <- [0..(size-1)] ]

patterns2' :: Int -> Cell-> [Cell]
patterns2' size (a, b) = all --drop (b * size + a) all
            where
                all = [ (x, y) | x <- [0..(size-1)], y <- [b..(size-1)] ]


format :: Board -> [[Int]]
format = map (map (\(_, x) -> if x then 1 else 0)) . transpose . groupBy ((==) `on` (fst . fst)) . sort

fill :: Int -> [Cell] -> Board
fill n cells = [ ((x, y), (x, y) `elem` cells) | x <- [0..(n - 1)], y <- [0..(n - 1)]]
