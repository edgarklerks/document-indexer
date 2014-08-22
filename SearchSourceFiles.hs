{-# LANGUAGE OverloadedStrings #-}
module Main where

import DSL.ElasticSearch
import DSL.Execute as E
import qualified DSL.Results as R
import Data.Monoid
import Control.Exception
import Control.Monad
import Control.Applicative
import Connection
import System.Environment
import Prelude hiding (map)
import Text.Printf
import Data.Maybe
import Tools



main :: IO ()
main = do
   xs <- getArgs
   when (null xs) $ error "Needs at least one or more tags, content or files to search (--help for more help)"
   let parsedQuery = parseThisQuery xs
   rs <- find_files $ parsedQuery
   case rs of
     Success xs -> showResults parsedQuery xs
     Failure k e -> error (show e)


showResults :: SourceQuery -> R.Value -> IO ()
showResults sq v = do
          let xs = R.parseResult v
          case xs of
            R.Error e  -> error $  "Couldn't parse results: " <> show e
            R.Success xs -> do
                printf "Total documents found: %d\nResults:\n" (R.total xs)
                forM_ (R.hits xs) $ \doc -> do
                    putStrLn "---------------------------------------------------------------------------------------"
                    let cf = fromJust $ R.fields doc
                    unless (null $ fromMaybe [] $ R.ctags cf ) $ do
                        printf "In file %-s we found the following tags:\n" (force R.path head cf)
                        putStrLn "---------------------------------------------------------------------------------------"
                        printf "%-20s %-10s %-10s %-60s\n" ("tagname" :: String) ("sourceline" :: String) ("offset" :: String) ("definition" :: String)
                        printf "-----------------------------------------------------\n"

                        forM_ (fromMaybe [] $ R.ctags cf) $ \tag ->
                           printf "%-20s %-10d %-10d %-60s\n" (force R.tag id tag) (force R.sourceline id tag) (force R.offset id tag) (force R.definition id tag)
                    let hs = fromMaybe [] $ join  $ fmap R.content <$> R.highlight $ doc
                    unless (null hs) $ do
                        printf "\nAnd highlighted areas:\n"
                        printf "----------------------\n"
                        forM_ hs $ \x -> printf "%s\n\n*******************NEXT*****************************\n\n" (replaceEm x)



force :: (a -> Maybe b) -> (b -> c) -> a -> c
force f g = g . fromJust . f


data SourceQuery = SourceQuery {
         tags :: [String],
         content :: [String],
         files :: [String]
  } deriving Show

data ParseMode = Tag | Content | Files | None


parseThisQuery = worker Tag (SourceQuery [] [] [])
     where worker tag sq t@(xs:xss) | xs == "--tags" = worker Tag sq xss
                                    | xs == "--content" = worker Content sq xss
                                    | xs == "--files" = worker Files sq xss
                                    | xs == "--help" = error "Usage: program --files search on or more files --content search the content --tags search the tags"
                                    | otherwise = case tag of
                                                    Tag -> worker tag (sq { tags = xs : tags sq }) xss
                                                    Content -> worker tag (sq { content = xs : content sq }) xss
                                                    Files -> worker tag (sq { files = xs : files sq }) xss
           worker tag sq _ = sq
find_files :: SourceQuery -> IO ESResult
find_files sq = do
    withConnection $ \c -> runESMonad c $ doQuery $ mq
           where mq = do
                 query XGet "documents/doc/_search?fields=path,ctags.tag,ctags.sourceline,ctags.offset,ctags.definition,project"
                 map $ do
                      "query" =>:
                                "bool" =>: do

                                     unless (null (content sq) && null (tags sq)) $
                                      "should" =>: ( list $ do
                                               unless  (null $ content sq) $
                                                forM_ (content sq) $ \t -> map $ "match" =>:  "content" =>: p t

                                               unless (null $ tags sq) $ forM_ ( tags sq) $ \t -> map $
                                                    "nested" =>: do
                                                        "path" =>: "ctags"
                                                        "query" =>: "match" =>: "ctags.tag" =>: p t
                                        )

                                     unless (null $ files sq) $
                                       "must" =>:   list ( forM_ (files sq) $ \t ->  "match" =>: "path" =>: p t )
                      "highlight" =>:
                          "fields" =>:
                             "content" =>: emptyMap
