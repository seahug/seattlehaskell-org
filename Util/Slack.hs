{-# LANGUAGE OverloadedStrings #-}

module Util.Slack
    ( ApiToken
    , Email
    , InviteUserResult
    , inviteUser
    ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Prelude hiding (error)

type Url = String
type Method = String
type ApiToken = String
type Email = String
type Args = [(B.ByteString, B.ByteString)]

postRequest :: Url -> ApiToken -> Args -> IO (Response L.ByteString)
postRequest methodUrl token args = do
    initReq <- parseUrl methodUrl
    let fullArgs = [ ("token", B.pack token) ] ++ args
        req = urlEncodedBody fullArgs initReq
    manager <- newManager tlsManagerSettings
    httpLbs req manager

postRequestBody :: Url -> ApiToken -> Args -> IO L.ByteString
postRequestBody methodUrl token args = responseBody <$> (postRequest methodUrl token args)

postMethod :: Method -> ApiToken -> Args -> IO L.ByteString
postMethod method token args =
    postRequestBody methodUrl token args
    where
        apiUrl :: Url
        apiUrl = "https://slack.com/api"
        methodUrl :: Url
        methodUrl = apiUrl ++ "/" ++ method

data UsersAdminInviteResponse = UsersAdminInviteResponse
    { ok :: Bool
    , error :: Maybe String
    }

instance FromJSON UsersAdminInviteResponse where
    parseJSON (Object v) = UsersAdminInviteResponse <$> v .: "ok" <*> v .:? "error"

data InviteUserResult =
    Done
    | ParseError
    | AlreadyInvited
    | AlreadyInTeam
    | InvalidEmail
    | InvalidAuth
    | UnknownError
    deriving Show

inviteUser :: ApiToken -> Email -> IO InviteUserResult
inviteUser token email = do
    let args = [ ("email", B.pack email), ("set_active", "true") ]
    body <- postMethod "users.admin.invite" token args
    return $ case decode body of
        Just r ->
            if ok r
            then Done
            else maybe UnknownError decodeError (error r)
        _ -> ParseError
    where
        decodeError :: String -> InviteUserResult
        decodeError s
            | s == "already_invited" = AlreadyInvited
            | s == "already_in_team" = AlreadyInTeam
            | s == "invalid_email" = InvalidEmail
            | s == "invalid_auth" = InvalidAuth
            | otherwise = UnknownError
