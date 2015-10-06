{-# LANGUAGE OverloadedStrings #-}

module Util.MeetupApi where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ConfigFile
import Data.Either.Utils
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX
import Data.Time.Format as DTF
import Data.Time.LocalTime
--import qualified Data.ByteString.Lazy as B
import Prelude
--import System.Locale

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
              <*> liftM2 zonedTimeHelper (v .: "utc_offset") (v .: "time")
        where
            zonedTimeHelper :: Int -> Int -> ZonedTime
            zonedTimeHelper utcOffset posixTime =
                let
                    timeZone = minutesToTimeZone (utcOffset `div` 1000 `div` 60)
                    utcTime = posixSecondsToUTCTime $ realToFrac (posixTime `div` 1000)
                in
                    utcToZonedTime timeZone utcTime

    parseJSON _ = mzero

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

formatZonedTime :: ZonedTime -> String
formatZonedTime t = formatTime DTF.defaultTimeLocale "%A %-d %B %Y at %H:%M UTC%z" t

formatEvent :: Event -> T.Text
formatEvent e = TL.toStrict $ TF.format "{} {} {}" [
        (eventTitle e)
      , (eventUrl e)
      , T.pack (formatZonedTime (eventZonedTime e))
    ]

meetupApiEventsUrl :: String -> String
meetupApiEventsUrl apiKey =
    TL.unpack $ TF.format "https://api.meetup.com/2/events?&sign=true&group_urlname=seahug&status=upcoming&page=1&key={}" [TL.pack apiKey]

meetupApiKey :: String -> IO String
meetupApiKey configFileName = do
    val <- readfile emptyCP configFileName
    let cp = forceEither val
    let apiKey = forceEither $ get cp "" "api_key"
    return apiKey

