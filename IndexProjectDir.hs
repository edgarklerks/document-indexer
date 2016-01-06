{-# LANGUAGE OverloadedStrings #-}
module Main where

import DSL.Execute
import DSL.ElasticSearch
import Etags.Parser
import System.Environment
import System.Process
import System.IO
import Control.Monad
import Data.Monoid
import Data.Maybe
import System.FilePath
import Prelude hiding (map)
import Connection
import System.Path
import Tools

main :: IO ()
main = do
    xs <- getArgs
    case xs of
      [dir, project] -> analyzeDir dir project
      _ -> error "Need a project dir and project name"

analyzeDir :: FilePath -> String -> IO ()
analyzeDir dir project = withTempDir project $ \d -> do
     let fp = d </> "ETAGS-OUT"
     (_,stdout,stderr,_) <- runInteractiveCommand $ "ctags-exuberant -f " <> fp <> " -e -R " <> dir
     xs <- hGetContents stderr
     ys <- hGetContents stdout
 -- For now force the input, so the process ran completely. We should use a conduit or a pipe to read files
     unless (null xs) $ print $ last xs
     unless (null ys) $ print $ last ys
     etagdump <- readEtagFile fp
     print etagdump
     when (isNothing etagdump) $ error "Couldn't read etag file"
     withConnection $ \co ->
      forM_ (fromJust etagdump) $ \section -> do
            let filepath = ("/"<>) $ fromJust $ absNormPath d  (srcfile section)
            putStrLn $ "Creating: " <> filepath
            contents <- readFile filepath

            res <- runESMonad co $ doQuery $ do
             query XPost "documents/doc?op_type=create"
             map $ do
               "project" =>: p project
               "type" =>: "sourcefile"
               "content" =>: p contents
               "path" =>: p filepath
               "ctags" =>: etagsFromSection section
            putStrLn $ "Result" <> show res

etagsFromSection :: Etag -> Query List ()
etagsFromSection section = list $ forM_ (tagdefinitions section) $ \tagdef -> do
                                map $ do
                                    "tag" =>: p (tagname tagdef)
                                    "sourceline" =>: p (linenumber tagdef)
                                    "offset" =>: p (byte_offset tagdef)
                                    "definition" =>: p (tagdefinition tagdef)
