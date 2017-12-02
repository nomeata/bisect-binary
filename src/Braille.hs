module Braille where

import Data.Char
import Data.Bits

dotsToBrailleBar :: [Int] -> String
dotsToBrailleBar [] = ""
dotsToBrailleBar [x] = [dotsToBrailleChar x 0]
dotsToBrailleBar (x:y:xs) = dotsToBrailleChar x y : dotsToBrailleBar xs

dotsToBrailleChar :: Int -> Int -> Char
dotsToBrailleChar n m =
    chr $ 0x2800 + sum
        [ bit 0 * fromEnum (testBit n 3)
        , bit 1 * fromEnum (testBit n 2)
        , bit 2 * fromEnum (testBit n 1)
        , bit 3 * fromEnum (testBit m 3)
        , bit 4 * fromEnum (testBit m 2)
        , bit 5 * fromEnum (testBit m 1)
        , bit 6 * fromEnum (testBit n 0)
        , bit 7 * fromEnum (testBit m 0)
        ]
