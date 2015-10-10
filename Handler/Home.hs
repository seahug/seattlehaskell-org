module Handler.Home where

import Data.Maybe
import Import
import qualified Prelude
import qualified Util.Formatting as F
import qualified Util.Meetup as M

getHomeR :: Handler Html
getHomeR = do
    event <- liftIO $ do
        settings <- M.readSettings "config/meetup.yml"
        events <- M.fetchEvents $ fromJust settings
        return $ Prelude.head events
    defaultLayout $ do
        setTitle "SeaHUG"
        $(widgetFile "home")

getAboutR :: Handler Html
getAboutR = do
    defaultLayout $ do
        setTitle "SeaHUG - about"
        $(widgetFile "about")

getContactR :: Handler Html
getContactR = do
    defaultLayout $ do
        setTitle "SeaHUG - contact"
        $(widgetFile "contact")

