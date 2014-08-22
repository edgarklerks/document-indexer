module Tools where

import Data.List.Utils
import System.Posix.Temp
import System.FilePath
import System.Directory
import Control.Concurrent.QSem
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception
import Control.Monad

replaceEm :: String -> String
replaceEm  = replace "<em>" "**" . replace "</em>" "**"

mktempDir :: String -> IO FilePath
mktempDir st = mkdtemp $ "/tmp" </> st

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir st = bracket (mktempDir st) removeDirectoryRecursive

concurrentForM_ :: [a] -> Int -> (a -> IO b) -> IO ()
concurrentForM_ as num_processes f = do
       qsem <- newQSem num_processes
       xs <- forM as $ \a -> do
            p <- newEmptyMVar
            forkIO . bracket_ (waitQSem qsem) (signalQSem qsem) . void $ (f a >> putMVar p ())
            return p
       waitOnThreads xs
  where waitOnThreads (p:ps) = do
           takeMVar p
           waitOnThreads ps
        waitOnThreads [] = return ()
-- | Creates a quantity semaphore to control the amount of processes created.
concurrentForM :: [a] -> Int -> (a -> IO b) -> IO [b]
concurrentForM as num_processes f = do
       qsem <- newQSem num_processes
       xs <- forM as $ \a -> do
            p <- newEmptyMVar
            forkIO . bracket_ (waitQSem qsem) (signalQSem qsem) . void $ (f a >>= \b ->  putMVar p b)
            return p
       waitOnThreads xs
  where waitOnThreads (p:ps) = do
           b <- takeMVar p
           rest <- waitOnThreads ps
           return (b:rest)
        waitOnThreads [] = return []
