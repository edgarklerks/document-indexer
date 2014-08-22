module Main where

import DSL.ElasticSearch
import DSL.Execute
import DSL.Results

import Control.Monad
import Control.Applicative

import System.Process
import System.IO
import Config


main :: IO ()
main = do
   xs <- getArgs
   case xs of
      [x] -> process_nix_dir x
       _ -> error "I need your nix directory to index it"


process_nix_dir :: IO ()
process_nix_dir = undefined
