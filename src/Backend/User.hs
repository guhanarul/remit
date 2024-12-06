{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Backend.User where

import Data.Text (Text)
import Data.Aeson (FromJSON , ToJSON)
import GHC.Generics (Generic)
import Core.Turing

createUserUrl :: Text
createUserUrl = "letter/create/user"

data User = User
    {
        userId :: Text,
        userName :: Text,
        email    :: Text,
        closeNets :: Maybe [User],
        letters   :: Maybe [Text]
    }
    deriving (Show , Generic)
instance FromJSON User
instance ToJSON User


data CreateUserReq = CreateUserReq
    {
        userId :: Text,
        userName :: Text,
        email    :: Text
    }
    deriving (Show , Generic)
instance FromJSON CreateUserReq
instance ToJSON CreateUserReq



createUserHandler :: CreateUserReq ->  Either ErrorG ResponseG 
createUserHandler _ = Left $ NotFound "This function is yet to be implemented"



