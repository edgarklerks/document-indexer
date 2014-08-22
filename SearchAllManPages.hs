{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
module Main where

import DSL.ElasticSearch
import DSL.Execute as E
import Data.Monoid
import Control.Exception
import Control.Monad
import Control.Applicative
import Connection
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Text as T
import Prelude hiding (map)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as S
import Text.Printf
import qualified Data.List as L
import Data.Char
import DSL.Results
import Tools
import Data.Typeable

data FieldNotForceableException = FieldNotForceableException String
         deriving (Show, Typeable)

instance Exception FieldNotForceableException

main :: IO ()
main = do
     xs <- getArgs
     when (null xs) $ error "Need at least one term"
     result <- withConnection $ \c ->
         runESMonad c $ doQuery $ man_page_query (unwords xs)
     case result of
       E.Success x -> do
             let xs = parseResult x
             case xs of
               A.Success a -> showDocs a
               A.Error e -> error $ show ("error", show e)
       E.Failure s e -> print (s, e)

ff e f x = case f x of
           Just a -> a
           Nothing -> throw $ FieldNotForceableException $ show (x, e)


removeLineEndings = unwords . lines
removeControlCodes = L.filter isPrint

showDocs :: Document -> IO ()
showDocs dc = let docs =  hits dc
              in do
                 printf "%-60s %-20s %-20s\n" ("title" :: String) ( "section" :: String ) ( "highlight" :: String)
                 forM_ docs $ \doc -> do
                       x <- try $ do
                             let field = ff "fields" fields doc
                             let hg = ff "highlight" highlight doc
                             printf "%-60s %-20s %-20s\n" (concat $ ff "title" title field) (concat $ ff "section" section field) (replaceEm $ removeControlCodes $ removeLineEndings $ head $ ff "content" content hg)

                       case x of
                          Left (FieldNotForceableException e) -> return () -- printf "doc failed: %s" (show doc)
                          Right a -> return ()
                          Left e -> print ("Unknown error", e)



man_page_query :: String -> Query Map ()
man_page_query terms = do
       query XGet "documents/doc/_search?fields=title,section&size=300&fq=type:manpage"
       map $ do
         "query" =>:
          "match" =>:
            "content" =>: do
             "query" =>: p terms
             "operator" =>: "or"
         "highlight" =>:
            "fields" =>:
               "content" =>: emptyMap
         -- "fields" =>: list ("title" >> "section")
