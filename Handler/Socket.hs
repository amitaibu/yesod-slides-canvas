module Handler.Socket where

import           Data.Aeson.Encode
import           Import
import           Yesod.WebSockets
import Data.Text.Lazy.Builder (toLazyText)



chatStream :: WebSocketsT Handler ()
chatStream = do
    let lesson = toJSON $ Lesson 1 Nothing
    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    sendTextData $ encode lesson
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    App { appBroadcastChannel = writeChan } <- getYesod
    readChan <- atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan

    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (\msg -> do
            users <- lift (runDB $ selectList [] [] :: Handler [Entity User])
            atomically $ writeTChan writeChan $ msg
        ))

sourceWSText :: MonadIO m => ConduitM i Text (WebSocketsT m) ()
sourceWSText = sourceWS

lessonToText :: Lesson -> Text
lessonToText = toStrict . toLazyText . encodeToTextBuilder . toJSON

getSocketR :: Handler Html
getSocketR = do
    webSockets chatStream
    defaultLayout $ do
          setTitle "WebSockets page"
          toWidget [hamlet|
            Socket's page
          |]
