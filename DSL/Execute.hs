{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module DSL.Execute (
       doQuery,
       ConnectionObject,
       ESResult (..),
       ESMonad,
       newConnection,
       closeConnection,
       runESMonad,

) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Data.Typeable
import DSL.ElasticSearch
import qualified Data.Aeson as A
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Word
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Status as C
import Control.Exception

type Hostname = String
type Basepath = String
type Port = Int


newConnection :: Hostname ->  Port -> IO ConnectionObject
newConnection h p = C.newManager C.defaultManagerSettings >>= \m -> do
                               newMVar (Connection m h p)
closeConnection :: ConnectionObject -> IO ()
closeConnection c = do
            c' <- takeMVar c
            C.closeManager $ manager c'

runESMonad :: MonadIO m => MVar Connection -> ESMonad m a -> m a
runESMonad c es = runReaderT ( unESMonad es ) c

oneShot ::  Hostname ->  Port -> Query Map () ->  IO ESResult
oneShot hst p q = bracket (newConnection hst p) closeConnection (\c -> runESMonad c (doQuery q))

type ConnectionObject = MVar Connection

data Connection = Connection {
     manager :: C.Manager,
     hostname :: String,
     port :: Int
  }

data RequestBuilderException = RequestBuilderException String
     deriving (Show, Typeable)

instance Exception RequestBuilderException

data ESResult = Failure C.Status B.ByteString
              | Success A.Value
    deriving Show


newtype ESMonad m a = ESMonad {
                unESMonad :: ReaderT ConnectionObject m a
    } deriving (Monad, Functor, Applicative, MonadIO, MonadTrans, MonadReader (MVar Connection))

buildRequest :: Connection -> QueryObject -> C.Request
buildRequest c q = case C.parseUrl builtURI of
                        Nothing -> throw $ RequestBuilderException "Couldn't create a uri from QueryObject and ConnectionoOject"
                        Just req -> req { C.requestBody = body, C.method = method q }
    where builtURI = "http://" <> hostname c <> ":" <> (show (port c)) <> "/" <>  B.unpack (urihandler q)
          body = case method q `elem` bodyBearing of
                              True ->  C.RequestBodyLBS (A.encode $ json q)
                              False -> C.RequestBodyLBS ""
          bodyBearing = ["POST", "PUT", "GET"]



doQuery :: MonadIO m => Query Map () -> ESMonad m ESResult
doQuery qc = do
        con <- ask
        liftIO $ withMVar con $ \con' -> do
            let req = buildRequest con' (buildQuery qc)

            resp <- try $ C.httpLbs req (manager con')
            case resp of
              Left (e :: C.HttpException) -> return $ Failure (C.Status 9000 "Exception") (B.pack $ show e)
              Right resp -> do
                let body = C.responseBody resp
                let status = C.responseStatus resp
                case BL.toStrict body of
                       body' -> return $ case abs(C.statusCode status - 200) < 100 of
                                 True -> case body `seq` (A.decodeStrict body') of
                                              Nothing -> Failure status ("Couldn't decode body" <> body')
                                              Just x -> Success x
                                 _ -> Failure status body'
