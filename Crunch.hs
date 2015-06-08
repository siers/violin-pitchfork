module Crunch where

import Data.Array.CArray
import Data.Array.IArray
import Data.Char
import Data.Complex
import Data.Functor
import Data.List
import Math.FFT.Base
import Text.Printf

import Pick

type Samps = [Char]

doublify :: Char -> Double
doublify = (/ 0x80) . fromIntegral . (subtract 0x80) . ord

calculate :: Samps -> CArray Int (Complex Double)
calculate = dftRC . (\l -> listArray (1, length l) l) . map doublify

visualize :: [Double] -> [String]
visualize = map (printf "%10.2f")

most = foldl1 max

outlier :: [Double] -> Maybe Int
outlier s =
    if (most s) - (mean s) > 250
    then elemIndex (most s) s
    else Nothing
