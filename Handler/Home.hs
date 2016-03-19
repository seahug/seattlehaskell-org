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
  let
    gitHubUrl = appGitHubUrl $ appSettings app
    meetupUrl = appMeetupUrl $ appSettings app
  defaultLayout $ do
    setTitle "SeaHUG - about"
    $(widgetFile "about")
