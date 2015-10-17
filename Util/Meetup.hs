{-# LANGUAGE OverloadedStrings #-}

module Util.Meetup
(
    Event (..)
  , Venue (..)
  , fetchEvents
) where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Either.Utils
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Yaml
import qualified Network.HTTP.Conduit as C
import Prelude

data EventList = EventList { eventListEvents :: [Event] }

instance FromJSON EventList where
    parseJSON (Object v) = EventList <$> v .: "results"
    parseJSON _ = mzero

data Event = Event {
    eventTitle       :: !T.Text
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
              <*> liftM2 makeZonedTime (v .: "utc_offset") (v .: "time")
    parseJSON _ = mzero

makeZonedTime :: Int -> Int -> ZonedTime
makeZonedTime utcOffset posixTime =
    let
        timeZone = minutesToTimeZone (utcOffset `div` 1000 `div` 60)
        utcTime = posixSecondsToUTCTime $ realToFrac (posixTime `div` 1000)
    in
        utcToZonedTime timeZone utcTime

data Venue = Venue {
    venueName    :: !T.Text
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

fetchEvents :: String -> IO [Event]
fetchEvents meetupApiKey = do
    content <- C.simpleHttp $ formatEventsUrl meetupApiKey
    return $ parseEventListJson content
    where
        formatEventsUrl :: String -> String
        formatEventsUrl k =
            TL.unpack $ TF.format \
                "https://api.meetup.com/2/events?&sign=true&group_urlname=seahug&status=upcoming&page=1&key={}" \
                [TL.pack k]
        parseEventListJson :: LBS.ByteString -> [Event]
        parseEventListJson content = eventListEvents $ fromRight ((A.eitherDecode content) :: Either String EventList)
