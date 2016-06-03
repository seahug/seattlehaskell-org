{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Meetup
    ( Event (..)
    , Venue (..)
    , fetchEvents
    ) where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           Data.Yaml
import qualified Network.HTTP.Conduit as C
import           Prelude

data EventList = EventList { eventListEvents :: [Event] }

instance FromJSON EventList where
    parseJSON (Object v) = EventList <$> v .: "results"
    parseJSON _ = mzero

data Event = Event
    { eventTitle       :: !T.Text
    , eventDescription :: !T.Text
    , eventVenue       :: Venue
    , eventUrl         :: !T.Text
    , eventZonedTime   :: ZonedTime
    }

instance FromJSON Event where
    parseJSON (Object v) =
        Event <$> v.: "name"
              <*> v.: "description"
              <*> v.: "venue"
              <*> v.: "event_url"
              <*> liftM2 mkZonedTime (v .: "utc_offset") (v .: "time")
    parseJSON _ = mzero

mkZonedTime :: Int -> Int -> ZonedTime
mkZonedTime utcOffset posixTime =
    let
        timeZone = minutesToTimeZone (utcOffset `div` 1000 `div` 60)
        utcTime = posixSecondsToUTCTime $ realToFrac (posixTime `div` 1000)
    in
        utcToZonedTime timeZone utcTime

data Venue = Venue
    { venueName    :: !T.Text
    , venueAddress :: !T.Text
    , venueCity    :: !T.Text
    , venueState   :: !T.Text
    }

instance FromJSON Venue where
    parseJSON (Object v) =
        Venue <$> v .: "name"
              <*> v .: "address_1"
              <*> v .: "city"
              <*> v .: "state"
    parseJSON _ = mzero

fetchEvents :: String -> String -> IO (Maybe [Event])
fetchEvents meetupApiUrl meetupApiKey = do
    maybeResponse <- fetch formatEventsUrl
    return $ maybeResponse >>= parseEventListJson
    where
        formatEventsUrl :: String
        formatEventsUrl =
            TL.unpack $ TF.format
                "{}/2/events?&sign=true&group_urlname=seahug&status=upcoming&page=1&key={}"
                (map TL.pack [meetupApiUrl, meetupApiKey])
        fetch :: String -> IO (Maybe BSL.ByteString)
        fetch url = fmap Just (C.simpleHttp url) `E.catch` (\(_ :: C.HttpException) -> return Nothing)
        parseEventListJson :: BSL.ByteString -> Maybe [Event]
        parseEventListJson = fmap eventListEvents . A.decode
