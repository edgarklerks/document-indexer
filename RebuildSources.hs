{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import System.Environment
import Config
import Shelly
import Data.Maybe
import Data.Either
import qualified Data.Text as T
default (T.Text)



main :: IO ()
main = do
     x <- getArgs
     case x of
       [] -> error "Need a configuration file"
       [x] -> do
          process_sources x

process_sources :: String -> IO ()
process_sources fp = do
            cf <- either error id <$> parseConfigFile fp
            let nix = getList "nix" "paths" cf
            let man = getList "man" "paths" cf
            let projects = getSection "projects" cf
            let path = getString "main" "execpath" cf
            shelly $ cmd ""

            when (isJust nix) $ rebuild_nix (fromJust nix)
            when (isJust man) $ rebuild_man_pages (fromJust man)
            when (isJust projects) $ rebuild_projects (fromJust projects)


rebuild_projects :: [(String,  String)] -> IO ()
rebuild_projects = undefined



rebuild_nix :: [String] -> IO ()
rebuild_nix xs = return ()

rebuild_man_pages :: [String] -> IO ()
rebuild_man_pages xs = shelly $ run_ "index_man_pages" (T.pack <$> xs)
