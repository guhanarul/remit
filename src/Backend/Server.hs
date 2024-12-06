{-# LANGUAGE OverloadedStrings #-}
module Backend.Server where

import Core.Turing (registerHandler , MethodT (..) , initRouteTableRef , RouteTable, computeReqWithHandlerG)
import Network.Wai (requestMethod , pathInfo , strictRequestBody, Application)
import Backend.User
import Data.Text (pack,unpack)
import Data.List (intercalate)

app :: Application
app request response = do
    _ <- initRouteTableRef endpoints
    let method = requestMethod request
        pathL = pathInfo request
        path = intercalate "/" (map unpack pathL)
    body <- strictRequestBody request
    computeReqWithHandlerG  body response ((pack $ show method) , (pack $ path)) --removing this pack for methods will fail computeKey (SOLVE THIS)

endpoints :: RouteTable ()
endpoints = do
    registerHandler Post createUserUrl createUserHandler

