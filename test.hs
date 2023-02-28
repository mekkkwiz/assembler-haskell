import Numeric ( showIntAtBase )
import Data.Char ( intToDigit )

padToLength :: Int -> a -> [a] -> [a]
padToLength n c xs
    | length xs >= n = xs
    | otherwise = padToLength n c (c:xs)

decToBin :: Int -> String
decToBin dec = showIntAtBase 2 intToDigit dec ""

twosComDecToBin :: Int -> Int -> String
twosComDecToBin dec bit
    | dec >= 0 = let
        bin1 = decToBin dec
        in padToLength bit '0' bin1
    | otherwise = let
        bin2 = decToBin (2^bit + dec)
        in bin2