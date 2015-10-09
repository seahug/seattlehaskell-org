{-# LANGUAGE OverloadedStrings #-}

module Util.MeetupApi
(
    Event (..)
  , MeetupSettings (..)
  , Venue (..)
  , getMeetupEvents
  , readMeetupSettings
) where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Yaml
import qualified Network.HTTP.Conduit as C
import Prelude

data EventList = EventList {
    eventListEvents :: [Event]
}

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

data MeetupSettings = MeetupSettings {
    meetupSettingsApiKey :: !T.Text
}

instance FromJSON MeetupSettings where
    parseJSON (Object v) =
        MeetupSettings <$> v .: "api_key"
    parseJSON _ = mzero

getMeetupEvents :: MeetupSettings -> IO [Event]
getMeetupEvents settings = do
    content <- C.simpleHttp $ meetupEventsUrl settings
    return $ parseEventListJson content

meetupEventsUrl :: MeetupSettings -> String
meetupEventsUrl settings =
    TL.unpack $ TF.format \
        "https://api.meetup.com/2/events?&sign=true&group_urlname=seahug&status=upcoming&page=1&key={}" \
        [meetupSettingsApiKey settings]

parseEventListJson :: LBS.ByteString -> [Event]
parseEventListJson content =
    case (A.eitherDecode content) :: Either String EventList of
        Left _ -> error "FAIL"
        Right eventList -> eventListEvents eventList

readMeetupSettings :: FilePath -> IO (Maybe MeetupSettings)
readMeetupSettings fileName = do
    value <- BS.readFile fileName
    return (decode value :: Maybe MeetupSettings)

