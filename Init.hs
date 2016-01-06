{-# LANGUAGE OverloadedStrings #-}
module Main where

import DSL.ElasticSearch
import DSL.Execute
import Control.Exception
import Control.Monad
import Prelude hiding (map)
import Control.Monad.Trans
import Connection

{-- Index and Mappings for the document indexer --}

main :: IO ()
main  = do
    xs <- withConnection actions
    print xs

actions c = do
         deleteIndex c
         createIndex c
         createDoc c

createDoc :: ConnectionObject -> IO ()
createDoc c = runESMonad c $ do
                liftIO $ ppQuery documentQuery
                xs <- doQuery $ documentQuery
                liftIO $ putStrLn "Result:"
                liftIO $ print xs

deleteIndex :: ConnectionObject -> IO ()
deleteIndex c = runESMonad c $ do
                   liftIO $ ppQuery deleteQuery
                   xs <- doQuery $ deleteQuery
                   liftIO $ putStrLn "Result:"
                   liftIO $ print xs
createIndex :: ConnectionObject -> IO ()
createIndex c = runESMonad c $ do
        liftIO $ ppQuery createQuery
        xs <- doQuery createQuery
        liftIO $ putStrLn "Result:"
        liftIO $ print xs

documentQuery :: Query Map ()
documentQuery = do
            query XPut "documents/_mappings/doc"
            map $

               "properties" =>: do
                  "content" =>: do
                     "type" =>:  "string"
                     "analyzer" =>:  "content_analyzer"
                  "project" =>:
                      "type" =>:  "string"
                  "section" =>:
                      "type" =>:  "string"
                  "title" =>:
                      "type" =>:  "string"
                  "path" =>:
                      "type" =>:  "string"
                  "ctags" =>: do
                      "type" =>:  "nested"
                      "properties" =>: do
                            "tag" =>: ("type" =>: "string")
                            "sourceline" =>: ("type" =>: "long")
                            "offset" =>: ("type" =>: "long")
                            "definition" =>: ("type" =>: "string")


deleteQuery :: Query Map ()
deleteQuery = query XDelete "documents"
createQuery :: Query Map ()
createQuery = do
             query XPut "documents"
             map $
                     "settings" =>:
                       "analysis" =>: do
                          "analyzer" =>:
                             "content_analyzer" =>: do
                                 "tokenizer" =>: "standard"
                                 "filter" =>:
                                     list (
                                            "standard"
                                        >>  "tokens"
                                        >>  "asciifolder" )
                                 "stopwords" =>:  "english"
                          "filter" =>: do
                                "tokens" =>:
                                  "type" =>:  "lowercase"
                                "asciifolder" =>: do

                                  "type" =>:  "asciifolding"
                                  "preserve_original" =>: p True
