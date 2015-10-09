module Handler.Home where

import Import
import Data.Time.Format as DTF
import Data.Time.LocalTime
import qualified Util.MeetupApi as M

-- $TODO: Use monad transformers or something to tidy this up
getNextMeetupEvent :: FilePath -> IO M.Event
getNextMeetupEvent configFileName = do
    result <- M.readMeetupSettings configFileName
    case result of
        Nothing -> error "FAIL"
        Just settings -> do
            events <- M.getMeetupEvents settings
            return $ case events of
                [] -> error "FAIL"
                event : _ -> event

formatZonedTime :: ZonedTime -> String
formatZonedTime t = formatTime DTF.defaultTimeLocale "%A %-d %B %Y at %H:%M UTC%z" t

getHomeR :: Handler Html
getHomeR = do
    nextMeetupEvent <- liftIO $ getNextMeetupEvent "config/meetup.yml"

    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "homepage")

