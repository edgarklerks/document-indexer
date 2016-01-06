module Main where

import DSL.Execute
import DSL.ElasticSearch
import Etags.Parser
import System.Environment

main :: IO ()
main = do
    xs <- getArgs
    case xs of
      [dir] -> analyzeDir

analyzeDir :: IO ()
analyzeDir = undefined
