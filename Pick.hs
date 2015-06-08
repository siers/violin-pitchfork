module Pick where

import Data.List
import Data.Functor
import Freq
import Crunch

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
violinpick as = off . map (as !!) . cents 20 <$> (take 4 . iterate (+7) $ 35)
