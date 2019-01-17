module Data.DTuple (
    Digit(..),
    DTuple,
    select,
    set,
    fill,
    toDigit,
    fromDigit,
    toDigits,
    fromDigits
) where

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Eq, Ord, Enum, Bounded)

newtype DTuple a = DTuple {tuple :: (a, a, a, a, a, a, a, a, a, a)}

instance Functor DTuple where
    fmap f (DTuple (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)) = DTuple (f x0, f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9)

select :: Digit -> DTuple a -> a
select D0 (DTuple (x, _, _, _, _, _, _, _, _, _)) = x
select D1 (DTuple (_, x, _, _, _, _, _, _, _, _)) = x
select D2 (DTuple (_, _, x, _, _, _, _, _, _, _)) = x
select D3 (DTuple (_, _, _, x, _, _, _, _, _, _)) = x
select D4 (DTuple (_, _, _, _, x, _, _, _, _, _)) = x
select D5 (DTuple (_, _, _, _, _, x, _, _, _, _)) = x
select D6 (DTuple (_, _, _, _, _, _, x, _, _, _)) = x
select D7 (DTuple (_, _, _, _, _, _, _, x, _, _)) = x
select D8 (DTuple (_, _, _, _, _, _, _, _, x, _)) = x
select D9 (DTuple (_, _, _, _, _, _, _, _, _, x)) = x

set :: Digit -> DTuple a -> a -> DTuple a
set D0 (DTuple (_, y1, y2, y3, y4, y5, y6, y7, y8, y9)) x = DTuple (x, y1, y2, y3, y4, y5, y6, y7, y8, y9)
set D1 (DTuple (y0, _, y2, y3, y4, y5, y6, y7, y8, y9)) x = DTuple (y0, x, y2, y3, y4, y5, y6, y7, y8, y9)
set D2 (DTuple (y0, y1, _, y3, y4, y5, y6, y7, y8, y9)) x = DTuple (y0, y1, x, y3, y4, y5, y6, y7, y8, y9)
set D3 (DTuple (y0, y1, y2, _, y4, y5, y6, y7, y8, y9)) x = DTuple (y0, y1, y2, x, y4, y5, y6, y7, y8, y9)
set D4 (DTuple (y0, y1, y2, y3, _, y5, y6, y7, y8, y9)) x = DTuple (y0, y1, y2, y3, x, y5, y6, y7, y8, y9)
set D5 (DTuple (y0, y1, y2, y3, y4, _, y6, y7, y8, y9)) x = DTuple (y0, y1, y2, y3, y4, x, y6, y7, y8, y9)
set D6 (DTuple (y0, y1, y2, y3, y4, y5, _, y7, y8, y9)) x = DTuple (y0, y1, y2, y3, y4, y5, x, y7, y8, y9)
set D7 (DTuple (y0, y1, y2, y3, y4, y5, y6, _, y8, y9)) x = DTuple (y0, y1, y2, y3, y4, y5, y6, x, y8, y9)
set D8 (DTuple (y0, y1, y2, y3, y4, y5, y6, y7, _, y9)) x = DTuple (y0, y1, y2, y3, y4, y5, y6, y7, x, y9)
set D9 (DTuple (y0, y1, y2, y3, y4, y5, y6, y7, y8, _)) x = DTuple (y0, y1, y2, y3, y4, y5, y6, y7, y8, x)

fill :: a -> DTuple a
fill x = DTuple (x, x, x, x, x, x, x, x, x, x)

toDigit :: Integral i => i -> Digit
toDigit d = case d of
                0 -> D0
                1 -> D1
                2 -> D2
                3 -> D3
                4 -> D4
                5 -> D5
                6 -> D6
                7 -> D7
                8 -> D8
                d -> D9

fromDigit :: Integral i => Digit -> i
fromDigit d = case d of
                D0 -> 0
                D1 -> 1
                D2 -> 2
                D3 -> 3
                D4 -> 4
                D5 -> 5
                D6 -> 6
                D7 -> 7
                D8 -> 8
                D9 -> 9
    
rdigits :: Integral i => i -> [Digit]
rdigits d = if d  < 10
              then [toDigit d]
              else (toDigit $ (d `mod` 10)):(rdigits $ (d `div` 10))

toDigits :: Integral i => i -> [Digit]
toDigits = reverse . rdigits . abs

fromDigits :: Integral i => [Digit] -> i
fromDigits = foldl (\z d -> 10 * z + (fromDigit d)) 0