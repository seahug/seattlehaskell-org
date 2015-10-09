module Handler.Home where

import Import
import Data.Time.Format as DTF
import Data.Time.LocalTime
import qualified Util.MeetupApi as M

formatZonedTime :: ZonedTime -> String
formatZonedTime t = formatTime DTF.defaultTimeLocale "%A %-d %B %Y at %H:%M UTC%z" t

getMeetupEvents' :: FilePath -> IO [M.Event]
getMeetupEvents' fileName = do
    result <- M.readMeetupSettings fileName
    case result of
        Nothing -> error "FAIL"
        Just settings -> M.getMeetupEvents settings

getMeetupEvents :: IO [M.Event]
getMeetupEvents = getMeetupEvents' "config/meetup.yml"

getNextMeetupEvent :: IO M.Event
getNextMeetupEvent = do
    events <- getMeetupEvents
    return $ case events of
        [] -> error "FAIL"
        event : _ -> event

getHomeR :: Handler Html
getHomeR = do
    nextMeetupEvent <- liftIO getNextMeetupEvent

    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "homepage")

