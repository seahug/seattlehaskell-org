
module Handler.Home where

import Import
import Data.Time.Format as DTF
import Data.Time.LocalTime
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format as TF
import qualified Util.MeetupApi as M
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, withSmallInput)

formatEvent :: M.Event -> T.Text
formatEvent e = TL.toStrict $ TF.format "{} {} {}" [
        (M.eventTitle e)
      , (M.eventUrl e)
      , T.pack (formatZonedTime (M.eventZonedTime e))
    ]

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

getMeetupEventDescriptions :: IO Text
getMeetupEventDescriptions = do
    events <- getMeetupEvents
    return $ case events of
        [] -> error "FAIL"
        event : _ -> formatEvent event

getNextMeetupEvent :: IO M.Event
getNextMeetupEvent = do
    events <- getMeetupEvents
    return $ case events of
        [] -> error "FAIL"
        event : _ -> event

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    --(formWidget, formEnctype) <- generateFormPost sampleForm
    --let submission = Nothing :: Maybe (FileInfo, Text)
    --    handlerName = "getHomeR" :: Text

    nextMeetupEvent <- liftIO getNextMeetupEvent

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    --((result, formWidget), formEnctype) <- runFormPost sampleForm
    --let handlerName = "postHomeR" :: Text
    --    submission = case result of
    --        FormSuccess res -> Just res
    --        _ -> Nothing

    nextMeetupEvent <- liftIO getNextMeetupEvent

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
