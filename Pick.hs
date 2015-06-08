{-# LANGUAGE BangPatterns #-}

module Pick where

import Data.List
import Data.Functor
import Freq

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- logorithmically reindex array whilst possibly
-- duplicating or losing stuff along the way
-- a good p is 0.003 if l = 4k
logopick :: Double -> [a] -> [a]
logopick p as = ((as !!) . reindex . fromIntegral) `fmap` enumFromTo 1 (floor target)
    where
        reindex = min (subtract 1 $ floor l) . floor . adjust . log
        l       = fromIntegral $ length as
        target  = l * p
        adjust  = (*) $ l / (log target)

-- G3 = 35, quint = +7.
violinpick :: [Double] -> [Double]
violinpick as = mean . map (as !!) . cents 20 <$> (take 4 . iterate (+7) $ 35)
