{-# LANGUAGE DeriveDataTypeable, GADTs, ScopedTypeVariables, DeriveGeneric, OverloadedStrings #-}

module DSL.Results (
       Tag(..),
       Document(..),
       Doc(..),
       Fields(..),
       parseResult,
       A.Result(..),
       A.Value --       getCTags
) where

import DSL.Execute
import DSL.ElasticSearch
import GHC.Generics
import qualified Data.Aeson as A
import Data.Aeson (fromJSON, Value, ( .: ), (.:?))
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as S
import Control.Applicative
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable
import Control.Exception

data Tag = Tag {
    tag :: Maybe String,
    sourceline :: Maybe Integer,
    offset :: Maybe Integer,
    definition :: Maybe String
 } deriving Show

{-- Exception hierarchy, so we can signal something is going wrong when parsing the results --}
data ResultException = forall e. Exception e => ResultException e
          deriving Typeable

instance Show ResultException where
     show (ResultException e) = show e

instance Exception ResultException

resultExceptionFromException :: Exception e => SomeException -> Maybe e
resultExceptionFromException e = fromException e >>= \(ResultException a) -> cast a

resultExceptionToException :: Exception e => e -> SomeException
resultExceptionToException = toException . ResultException

data CTagsNotParseableException = CTagsNotParseableException String
     deriving (Typeable, Show)


instance Exception CTagsNotParseableException where
         toException = resultExceptionToException
         fromException = resultExceptionFromException

data ResultNotParseableException = ResultNotParseableException String
     deriving (Show, Typeable)

instance Exception ResultNotParseableException where
         toException = resultExceptionToException
         fromException = resultExceptionFromException

instance A.FromJSON Tag where
  parseJSON (A.Object v) = Tag <$> v .:? "ctags.tag"
                               <*> v .:? "ctags.sourceline"
                               <*> v .:? "ctags.offset"
                               <*> v .:? "ctags.definition"

data Document = Document {
       hits :: [Doc],
       total :: Integer
 } deriving (Show, Generic)

instance A.FromJSON Document

data Fields = Fields {
      project :: Maybe [String],
      content :: Maybe [String],
      title :: Maybe [String],
      path :: Maybe [String],
      section :: Maybe [String],
      ctags :: Maybe [Tag]
        } deriving Show

instance A.FromJSON Fields where
   parseJSON t@(A.Object v) = Fields <$> v .:? "project"
                                     <*> v .:? "content"
                                     <*> v .:? "title"
                                     <*> v .:? "path"
                                     <*> v .:? "section"
                                     <*> parseCTags v


parseCTags :: S.HashMap T.Text Value -> A.Parser (Maybe [Tag])
parseCTags h = let tag = S.lookup "ctags.tag" h
                   sourceline = S.lookup "ctags.sourceline" h
                   offset = S.lookup "ctags.offset" h
                   definition = S.lookup "ctags.definition" h
               in return $ parseDist Tag tag sourceline offset definition

parseDist :: (A.FromJSON a, A.FromJSON b, A.FromJSON c, A.FromJSON d) => (a -> b -> c -> d -> e)
          -> Maybe Value
          -> Maybe Value
          -> Maybe Value
          -> Maybe Value
          -> Maybe [Tag]
parseDist f tag src off def | any isJust [tag, src, off, def] = Just $ L.zipWith4 Tag (distParse tag :: [Maybe String])
                                                                     (distParse src :: [Maybe Integer])
                                                                     (distParse off :: [Maybe Integer])
                                                                     (distParse def :: [Maybe String])

                            | otherwise = Nothing

distParse :: (A.FromJSON b) => Maybe Value -> [Maybe b]
distParse (Just x) = case A.fromJSON x of
                         A.Error e -> throw $ CTagsNotParseableException e
                         A.Success xs -> xs
distParse Nothing = repeat Nothing

data Doc = Doc {
      _id :: Maybe String,
      fields :: Maybe Fields,
      highlight :: Maybe Fields
   } deriving (Show, Generic)

instance A.FromJSON Doc

{-- This is fucking ugly and I don't need IO at all --}
parseResult :: Value -> A.Result Document
parseResult q = let b = fromJSON q :: A.Result (S.HashMap String Value)
                in case b of
                  A.Error e -> throw $ ResultNotParseableException e
                  A.Success a -> case S.lookup "hits" a of
                                    Just a -> case A.fromJSON a of
                                                       A.Success a -> case S.lookup ("hits" :: String) a of
                                                                            Just h -> Document <$> A.fromJSON h <*> case S.lookup "total" a of
                                                                                                                                  Nothing -> pure 0
                                                                                                                                  Just a -> case A.fromJSON a of
                                                                                                                                                A.Success a -> pure a
                                                                                                                                                A.Error e -> pure 0
                                                                            Nothing -> throw $ ResultNotParseableException "hits.hits is not defined in json"
                                                       A.Error e -> throw $ ResultNotParseableException e

                                    Nothing -> throw $ ResultNotParseableException "hits is not defined in json"
