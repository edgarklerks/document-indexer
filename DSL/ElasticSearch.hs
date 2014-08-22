{-# LANGUAGE GADTs, DeriveDataTypeable, FlexibleInstances, EmptyDataDecls, DeriveFunctor, GeneralizedNewtypeDeriving, RankNTypes, OverloadedStrings #-}
module DSL.ElasticSearch (
  buildQuery,
  QueryObject(..),
  p,
  s,
  i,
  d,
  map,
  list,
  query,
  ppQuery,
  emptyMap,
  (=>:),
  Label,
  List,
  Map,
  Closed,
  Attrib,
  QueryType(..),
  Query


  ) where

import Control.Monad
import Control.Monad.Free
import Control.Arrow
import Control.Exception hiding (Handler)
import Data.Typeable
import Data.Monoid
import Data.Aeson hiding (json)
import Control.Applicative
import Prelude hiding (map)
import Data.String
import qualified Data.Text as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as S
import qualified Data.Vector as V
import qualified Data.Aeson.Encode.Pretty as P
import GHC.Exts

type Label = L.Text
data List
data Map
data Scalar
data Closed
data Internal
data Attrib

data QueryObject = QueryObject {
      urihandler :: B.ByteString,
      method :: B.ByteString,
      json :: Value
    } deriving Show

data QueryType = XPost | XPut | XDelete | XGet | XHead
   deriving Show
type Handler = B.ByteString

data ESStruct q next = QueryModifier QueryType Handler next
                     | List q next
                     | Map q next
                     | Attrib (Label, q) next
                     | Scalar Value next
   deriving (Functor, Show)

newtype Query t a = Query {
    unQuery :: Free (ESStruct (Query Internal ())) a
    } deriving (Functor, Monad, Applicative, MonadFree (ESStruct (Query Internal ())), Show)

data QueryException = forall e. Exception e => QueryException e
     deriving Typeable

instance Show QueryException where
     show (QueryException e) = show e

queryExceptionFromException :: Exception e => SomeException -> Maybe e
queryExceptionFromException e = fromException e >>= \(QueryException a) -> cast a

queryExceptionToException :: Exception e => e -> SomeException
queryExceptionToException = toException . QueryException

instance Exception QueryException

data BuildListException = BuildListException String
     deriving (Show, Typeable)

instance Exception BuildListException where
         fromException = queryExceptionFromException
         toException = queryExceptionToException

-- instance IsString (Query w ()) where
--        fromString x = Query (Free (Scalar (toJSON x) (Pure ())))

instance IsString (Query w a) where
       fromString x = Query (Free (Scalar (toJSON x) (Pure undefined)))

instance Num (Query w ()) where
       fromInteger x = Query (Free (Scalar (toJSON x) (Pure ())))

instance Fractional (Query w ()) where
       fromRational x = Query (Free (Scalar (toJSON x) (Pure ())))

castQuery :: Query w a -> Query v a
castQuery (Query a) = Query a

emptyMap :: Query Map ()
emptyMap = Query (Free (Map (Query (Pure ())) (Pure ())))

p :: ToJSON a => a -> Query Scalar ()
p a = liftF (Scalar (toJSON a) ())

s :: L.Text -> Query Scalar ()
s = p

i :: Integer -> Query Scalar ()
i = p

d :: Double -> Query Scalar ()
d = p

list :: Query w () -> Query List ()
list s = liftF (List (castQuery s) ())

map :: Query Attrib () -> Query Map ()
map s = liftF (Map (castQuery s)  () )

(=>:) :: Label -> Query w () -> Query Attrib ()
(=>:) s q = liftF (Attrib (s, castQuery q) ())

infixr 1 =>:

query :: QueryType -> Handler -> Query Map ()
query qt h = liftF (QueryModifier qt h ())



test = do
  query XPut "test"
  map $ do
    "a" =>: (list $ do
            i (1 :: Integer)
            i 2
            i 3
            i 4)
    "c" =>: d 2
    "d" =>: do
      map $ do
        "abc" =>: d 2


buildQuery :: Query Map () -> QueryObject
buildQuery (Query p) = case p of
                        (Free (QueryModifier method handler k)) -> QueryObject { method = queryTypeToByteString method, urihandler = handler , json = buildQuery' k }


ppQuery :: Query Map () -> IO ()
ppQuery qq = let qo = buildQuery qq
             in do B.putStrLn ("Method: " <> method qo)
                   B.putStrLn ("Handler: " <> (urihandler qo))
                   B.putStrLn "Query:"
                   BL.putStrLn (P.encodePretty $ json qo)

buildQuery' :: Free (ESStruct (Query w1 ())) t -> Value
buildQuery' (Free (QueryModifier method handler k)) = buildQuery' k
buildQuery' (Free (Map q _)) = Object (S.fromList $ buildMap $ unQuery q)
buildQuery' (Free (List q _)) = Array $ V.fromList $ buildList (unQuery q)
buildQuery'  (Free (Scalar q _)) = q
buildQuery' t@(Free (Attrib q k)) = Object $ S.fromList $ buildMap (Free (Attrib (second castQuery q) k))
buildQuery' (Pure _) = Null


buildList :: (Show s, Show t) => Free (ESStruct (Query w1 s)) t -> [Value]
buildList (Free (Attrib q k)) = (Object $ S.fromList (buildMap (Free (Attrib (second castQuery q) k) ))) : buildList k
buildList (Free (List q k)) = (Array $ V.fromList $ buildList (unQuery q)) : buildList k
buildList (Free (Scalar q k)) = q : buildList k
buildList (Free (Map q k)) = (Object $ S.fromList $  buildMap $ unQuery q) : buildList k
buildList (Pure _) = []
buildList k = throw $ BuildListException $ "Cannot build list from: " <> (show k)

buildMap :: Free (ESStruct (Query w1 s)) t -> [(Label, Value)]
buildMap (Free (Attrib (s, q) k)) = (s, buildQuery' $ unQuery q) : buildMap k
buildMap (Pure _) = []

queryTypeToByteString :: QueryType -> B.ByteString
queryTypeToByteString XPost = "POST"
queryTypeToByteString XGet = "GET"
queryTypeToByteString XPut = "PUT"
queryTypeToByteString XDelete = "DELETE"
queryTypeToByteString XHead = "HEAD"
