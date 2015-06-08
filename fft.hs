import Data.Complex
import Data.Array.CArray
import Data.List.Split

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import GHC.IO.Handle
import GHC.IO.Handle.FD

import Crunch
import Pick

-- import Debug.Trace
-- t :: Show a => a -> a
-- t = trace =<< show

buffer = 8000 :: Int -- 4kHz

store :: MVar Samps -> IO ()
store var = do
    mapM_ store . chunksOf buffer =<< getContents

    where
        store content = (\f -> f var content) . how =<< isEmptyMVar var
        how e = if e then putMVar else (void .) . swapMVar

crunch  = violinpick . map (realPart . abs) . elems . calculate
analyze = print . visualize . crunch
listen  = putStr . maybe "" ((map (: "\n") "1234") !!) . outlier . crunch

live action = do
    lastChunk <- newEmptyMVar :: IO (MVar Samps)
    forkIO . forever $ readMVar lastChunk >>= action >> hFlush stdout
    store lastChunk

sequential action = mapM_ action . chunksOf buffer =<< getContents

main = do
    hSetBinaryMode stdin True
    live listen
