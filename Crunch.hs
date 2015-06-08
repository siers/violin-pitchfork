{-# LANGUAGE BangPatterns #-}

module Crunch where

import Data.Array.CArray
import Data.Array.IArray
import Data.Char
import Data.Complex
import Data.Functor
import Data.List
import Math.FFT.Base
import Text.Printf

type Samps = [Char]

doublify :: Char -> Double
doublify = (/ 0x80) . fromIntegral . (subtract 0x80) . ord

calculate :: Samps -> CArray Int (Complex Double)
calculate = dftRC . (\l -> listArray (1, length l) l) . map doublify

visualize :: [Double] -> [String]
visualize = map (printf "%10.2f")

-- |Numerically stable mean from statistics package
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

most = foldl1 max

off :: [Double] -> Double
off s = (most s) - (mean s)

outlier :: [Double] -> Maybe Int
outlier s =
    if off s > 100
    then elemIndex (most s) s
    else Nothing
