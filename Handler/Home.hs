module Handler.Home where

import Import
import qualified Prelude
import qualified Util.Formatting as F
import qualified Util.Meetup as M

getHomeR :: Handler Html
getHomeR = do
    app <- getYesod
    let meetupApiKey = appMeetupApiKey $ appSettings app
    event <- liftIO $ do
        events <- M.fetchEvents meetupApiKey
        return $ Prelude.head events
    defaultLayout $ do
        setTitle "SeaHUG"
        $(widgetFile "home")

getAboutR :: Handler Html
getAboutR = do
    app <- getYesod
    let gitHubUrl = appGitHubUrl $ appSettings app
    defaultLayout $ do
        setTitle "SeaHUG - about"
        $(widgetFile "about")

getContactR :: Handler Html
getContactR = do
    defaultLayout $ do
        setTitle "SeaHUG - contact"
        $(widgetFile "contact")

getPcphR :: Handler Html
getPcphR = do
    defaultLayout $ do
        setTitle "SeaHUG - Parallel and concurrent programming in Haskell"
        $(widgetFile "pcph")

