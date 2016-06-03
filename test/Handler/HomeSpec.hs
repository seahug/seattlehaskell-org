{-# LANGUAGE OverloadedStrings #-}

module Handler.HomeSpec (spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Types
                    ( Query
                    )
import           Network.Wai
                    ( Request
                    , pathInfo
                    , queryString
                    )

import TestImport
import TestUtil.TestServer hiding (withTestServer)

data State =
    AwaitingRequest |
    Success
    deriving Eq

type StateVar = TVar State

newStateVar :: State -> IO StateVar
newStateVar = newTVarIO

type ExpectedQueryItemList = [(B.ByteString, B.ByteString)]

checkExpectedQueryItems :: Request -> ExpectedQueryItemList -> Bool
checkExpectedQueryItems req = helper (queryString req)
    where
        helper :: Query -> ExpectedQueryItemList -> Bool
        helper _ [] = True
        helper query ((ek, ev) : qs) = case lookup ek query of
            Just maybeV -> case maybeV of
                Just v -> ev == v && helper query qs
                _ -> False
            _ -> False

requestHandler :: StateVar -> Request -> STM RequestHandlerResult
requestHandler sVar req = case pathInfo req of
    ["2", "events"] -> checkState [("key", "MEETUP-API-KEY"), ("group_urlname", "seahug")] AwaitingRequest Success True eventsJson
    _ -> return BadRequest
    where
        checkState :: ExpectedQueryItemList -> State -> State -> Bool -> L.ByteString -> STM RequestHandlerResult
        checkState expectedQueryItems expectedState nextState shouldShutDown responseText =
            if (not $ checkExpectedQueryItems req expectedQueryItems)
            then return BadRequest
            else do
                state <- readTVar sVar
                if state == expectedState
                then do
                    writeTVar sVar nextState
                    return $ if shouldShutDown then Done responseText else Valid responseText
                else return BadState
        eventsJson :: L.ByteString
        eventsJson = "{\"results\":[{\"utc_offset\":-25200000,\"venue\":{\"country\":\"us\",\
                \"\"localized_country_name\":\"USA\",\"city\":\"Kirkland\",\
                \"\"address_1\":\"720 Fourth Avenue\",\
                \"\"name\":\"Tableau Software, Central Way Plaza\",\
                \"\"lon\":-122.196175,\"id\":20325752,\"state\":\"WA\",\
                \"\"lat\":47.678707,\"repinned\":false},\"headcount\":0,\
                \"\"visibility\":\"public\",\"waitlist_count\":0,\
                \"\"created\":1360811043000,\"maybe_rsvp_count\":0,\
                \"\"description\":\"<p>DESCRIPTION<\\/p>\",\
                \"\"event_url\":\"http:\\/\\/www.meetup.com\\/seahug\\/events\\/1\\/\",\
                \"\"yes_rsvp_count\":6,\"announced\":false,\
                \"\"name\":\"General discussion\",\"id\":\"dtcnkfyvjbxb\",\
                \"\"time\":1466280000000,\"updated\":1444608130000,\
                \"\"group\":{\"join_mode\":\"open\",\"created\":1353609124000,\
                \"\"name\":\"Seattle Area Haskell Users' Group\",\
                \"\"group_lon\":-122.19000244140625,\"id\":5956362,\
                \"\"urlname\":\"SEAHUG\",\"group_lat\":47.689998626708984,\
                \"\"who\":\"Haskellers\"},\"status\":\"upcoming\"}],\
                \"\"meta\":{\"next\":\"https:\\/\\/api.meetup.com\\/2\\/events/id\",\
                \"\"method\":\"Events\",\"total_count\":12,\
                \"\"link\":\"https:\\/\\/api.meetup.com\\/2\\/events\",\"count\":1,\
                \"\"description\":\"DESCRIPTION\",\"lon\":\"\",\
                \"\"title\":\"Meetup Events v2\",\
                \"\"url\":\"https:\\/\\/api.meetup.com\\/2\\/events/id\",\
                \"\"signed_url\":\"https:\\/\\/api.meetup.com\\/2\\/events/id\",\
                \"\"id\":\"\",\"updated\":1444608130000,\"lat\":\"\"}}\n"

withTestServerHelper :: Port -> (StateVar -> YesodExample App ()) -> YesodExample App ()
withTestServerHelper port action = do
    sVar <- liftIO $ (newStateVar AwaitingRequest)
    serverInfo <- liftIO $ startTestServer port (requestHandler sVar)
    result <- action sVar
    liftIO $ stopTestServer serverInfo
    return result

spec :: Spec
spec = withApp $ do
    -- TODO: Figure out how to get port number from test-settings.yml
    --url <- appMeetupApiUrl . appSettings <$> getTestYesod
    let port = 4000
        withTestServer = withTestServerHelper port

    it "loads the index and checks it looks right" $ withTestServer $ \sVar -> do
        get HomeR
        statusIs 200
        htmlAllContain "h1" "SeaHUG"
        state <- liftIO $ readTVarIO sVar
        assertEqual "Test API server must be in success state" Success state

    -- This is a simple example of using a database access in a test.  The
    -- test will succeed for a fresh scaffolded site with an empty database,
    -- but will fail on an existing database with a non-empty user table.
    it "leaves the user table empty" $ withTestServer $ \sVar -> do
        get HomeR
        statusIs 200
        users <- runDB $ selectList ([] :: [Filter User]) []
        assertEqual "user table empty" 0 $ length users
        state <- liftIO $ readTVarIO sVar
        assertEqual "Test API server must be in success state" Success state
