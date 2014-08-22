{-# LANGUAGE OverloadedStrings #-}
module Main where

import DSL.ElasticSearch
import DSL.Execute
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import System.FilePath.Glob
import System.Environment
import qualified System.FilePath as F
import qualified Prelude as P
import Prelude hiding (map)
import System.Process
import System.IO
import Control.Exception
import Connection
import Tools

main :: IO ()
main = do
    xs <- getArgs
    when (null xs) $ error "Need man page directories to scan them"
    ps <- concat `fmap` forM xs get_man_dirs
    withConnection $ process_man_dirs ps


process_man_dirs :: [P.FilePath] -> ConnectionObject -> IO ()
process_man_dirs fps co = concurrentForM_ fps 3 $ \fp -> do

                                            names <- namesMatching (fp F.</> "*")
                                            concurrentForM_ names 6 $ \fp -> do
                                              void $ withTempDir "man" $ \d -> do
                                                let outfile = d F.</> "out"
                                                let (title, section) = F.splitExtension $ F.takeBaseName fp

                                                (stdin, stdout, stderr, ph) <- runInteractiveCommand $ unwords ["gunzip", fp, "-c |", "groff", " -Tascii", " > " <> outfile ]
                                                out <- hGetContents stdout
                                                err <- hGetContents stderr
                                                evaluate (length out)
                                                evaluate (length err)

                                                -- putStrLn $ "Creating: " <> title
                                                -- unless (null err) $ putStrLn ("Warning: " <> title <> " possibly failed to index, because of: " <> err )
                                                txt <- readFile outfile
                                                -- putStrLn $ "Text-length: " <> show (length txt)
                                                runESMonad co $ doQuery $ do
                                                     query XPost "documents/doc?op_type=create"
                                                     map $ do
                                                        "type" =>: "manpage"
                                                        "content" =>: p txt
                                                        "section" =>: p (drop 1 section)
                                                        "title" =>: p title
                                                        "path" =>: p fp
                                                -- liftIO $ print res
                                                -- liftIO $ putStrLn "Created.."







get_man_dirs :: P.FilePath -> IO [P.FilePath]
get_man_dirs fp = namesMatching (fp F.</> "man*")
