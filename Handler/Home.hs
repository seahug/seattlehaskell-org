module Handler.Home where

import Import
import qualified Util.Formatting as F
import qualified Util.Meetup as M

-- $TODO: Use monad transformers or something to tidy this up
getNextMeetupEvent :: FilePath -> IO M.Event
getNextMeetupEvent configFileName = do
    result <- M.readSettings configFileName
    case result of
        Nothing -> error "FAIL"
        Just settings -> do
            events <- M.fetchEvents settings
            return $ case events of
                [] -> error "FAIL"
                event : _ -> event

getHomeR :: Handler Html
getHomeR = do
    event <- liftIO $ getNextMeetupEvent "config/meetup.yml"
    defaultLayout $ do
        aDomId <- newIdent
        $(widgetFile "homepage")

