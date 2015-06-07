{-# LANGUAGE BangPatterns #-}

import Data.Array.CArray
import Data.Array.IArray
import Data.Complex
import Math.FFT.Base

import Data.Binary.Get
import Data.Bits
import Data.Char
import Data.Functor
import Data.List
import Data.List.Split
import Data.Word
import Text.Printf
import qualified Data.ByteString.Lazy.Char8 as BSC8

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import GHC.IO.Handle
import GHC.IO.Handle.FD

import Debug.Trace

-- t :: Show a => a -> a
-- t = trace =<< show

type Samps = [Char]
type Freqs = [Double]

buffer = 8000 :: Int -- 4kHz

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

freq :: Double -> Double
freq n = 2 ** ((n - 49) / 12) * 440

-- generate frequencies around a freq number
cents d n = [floor $ freq (n-d/100)..floor $ freq (n+d/100)]

doublify :: Char -> Double
doublify = (/ 0x80) . fromIntegral . (subtract 0x80) . ord 

calculate :: Samps -> CArray Int (Complex Double)
calculate = dftRC . (\l -> listArray (1, length l) l) . map doublify

visualize :: Freqs -> [String]
visualize = map (printf "%10.2f")

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
violinpick :: Freqs -> Freqs
violinpick as = mean . map (as !!) . cents 20 <$> (take 4 . iterate (+7) $ 35)

store :: MVar Samps -> IO ()
store var = do
    mapM_ store . chunksOf buffer =<< getContents

    where
        store content = (\f -> f var content) . how =<< isEmptyMVar var
        how e = if e then putMVar else (void .) . swapMVar

analyze =
    print . visualize . violinpick .
    map (realPart . abs) . elems . calculate

live = do
    lastChunk <- newEmptyMVar :: IO (MVar Samps)
    forkIO . forever $ readMVar lastChunk >>= analyze >> hFlush stdout
    store lastChunk

sequential = mapM_ analyze . chunksOf buffer =<< getContents

main = do
    hSetBinaryMode stdin True
    live
