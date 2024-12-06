{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Core.Turing where

import Prelude hiding (lookup,drop,take,length)
import Data.Text (Text ,pack, toUpper,drop,take,length)
import Data.Map (Map , empty , insert , lookup)
import Control.Monad.State
import Data.IORef 
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (Exception)
import Data.Aeson as A
import Network.Wai (responseLBS , Response,ResponseReceived)
import Network.HTTP.Types (status200 , status400 , status404 , status500 ,status502 )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Data.Aeson.Types (parseEither)

 

text2LazyByteS :: Text -> BL.ByteString
text2LazyByteS text = BL.fromStrict $ TE.encodeUtf8 text

data ErrorG = BadRequest Text
           | InternalError Text
           | NotFound Text
    deriving (Show)
instance Exception ErrorG

type ResponseG = A.Value

data Handler where
    Handler :: (FromJSON a , ToJSON a) => (a -> (Either ErrorG ResponseG)) -> Handler

throwErrorG :: Int -> Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
throwErrorG code message respond = 
    case code of
        400 -> respond $ responseLBS status400 [("Content-Type" , "text/plain")] ("BadRequest Error :" <> text2LazyByteS message)
        404 -> respond $ responseLBS status404 [("Content-Type" , "text/plain")] ("Not Found :" <> text2LazyByteS message)
        500 -> respond $ responseLBS status500 [("Content-Type" , "text/plain")] ("Internal Server Error :" <> text2LazyByteS message)
        _ -> respond $ responseLBS   status502 [("Content-Type" , "text/plain")] ("Unkown Error Thalaiva:" <> text2LazyByteS message)

data MethodT  = Post | Get
    deriving (Show)


type RouteTable = State (Map Text Handler)

routeTableRef :: IORef (Map Text Handler)
routeTableRef = unsafePerformIO $ newIORef empty
{-# NOINLINE routeTableRef #-}

initRouteTableRef :: (RouteTable ()) -> IO ()
initRouteTableRef endpoints = writeIORef routeTableRef $ execState endpoints empty

computeKey ::(Text,Text) -> Text
computeKey (k,v) = do
    let rf = drop 3 k
        rl = take (length rf - 3) rf --Still this is not pure!
    toUpper (rl <> v)
    
computeReqWithHandlerG :: BL.ByteString -> (Response -> IO ResponseReceived) -> (Text, Text) -> IO ResponseReceived
computeReqWithHandlerG body response keyD = do
    case decode body :: Maybe A.Value of
        Nothing -> throwErrorG 400 ("Invalid user Request Format" :: Text) response
        Just apiRequest -> do
            routetable <- readIORef routeTableRef
            let key = computeKey keyD
            putStrLn $ show key
            case lookup key routetable of
                Just (Handler (f :: a -> Either ErrorG ResponseG)) ->
                    case parseEither (parseJSON @a) apiRequest of
                        Left err -> throwErrorG 400 ("JSON parse error: " <> pack err) response
                        Right userReq -> runhandler f userReq
                Nothing -> throwErrorG 500 ("The handler is not found in RouteTable" :: Text) response
  where
    runhandler :: (a -> Either ErrorG ResponseG) -> a -> IO ResponseReceived
    runhandler f userReq = do
        case f userReq of
            Left (BadRequest err) -> throwErrorG 400 err response
            Left (InternalError err) -> throwErrorG 500 err response
            Left (NotFound err) -> throwErrorG 404 err response
            Right json -> response $ responseLBS status200 [("Content-Type", "application/json")] (A.encode json)


registerHandler :: (FromJSON a , ToJSON a ) => MethodT -> Text -> (a -> (Either ErrorG ResponseG)) -> RouteTable ()
registerHandler method url handler = modify $ insert ( computeKey (pack (show method), url)) (Handler handler)

data DefaultHandler = DefaultHandler deriving (Show)

instance FromJSON DefaultHandler where
    parseJSON _ = pure $ DefaultHandler
instance ToJSON DefaultHandler where
    toJSON  _ = "DefaultHandler"


norouteFallBack :: Handler
norouteFallBack = Handler $ \ (_ :: DefaultHandler) -> (Left $ InternalError "The Handler is not found")


