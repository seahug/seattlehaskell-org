module Handler.Home
    ( getAboutR
    , getHomeR
    ) where

import           Import

getHomeR :: Handler Html
getHomeR = do
    app <- getYesod
    let meetupUrl = appMeetupUrl $ appSettings app
    defaultLayout $ do
        setTitle "SeaHUG"
        $(widgetFile "home")

getAboutR :: Handler Html
getAboutR = do
    app <- getYesod
    let gitHubUrl = appGitHubUrl $ appSettings app
        meetupUrl = appMeetupUrl $ appSettings app
    defaultLayout $ do
        setTitle "SeaHUG - about"
        $(widgetFile "about")
