
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Data.Aeson
import Network.HTTP.Conduit (simpleHttp)
import qualified Util.MeetupApi as M

getMeetupEventDescriptions :: IO Text
getMeetupEventDescriptions = do
    apiKey <- M.meetupApiKey "config/meetup.conf"
    let url = M.meetupApiEventsUrl apiKey
    let content = simpleHttp url
    d <- (eitherDecode <$> content) :: IO (Either String M.EventList)
    return $ case d of
        Left _ -> "(error)"
        Right es -> case M.eventListEvents es of
            [] -> ""
            e : _ -> M.formatEvent e

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text

    eventDescription <- liftIO getMeetupEventDescriptions
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    eventDescription <- liftIO getMeetupEventDescriptions
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
